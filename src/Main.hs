{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Development.Redo.Util

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import Data.List
import Data.List.Split
import Data.Typeable
import SimpleGetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

-- | A type for redo targets
-- This should be constructed from 'redoTarget' or 'redoTargetFromDir'.
newtype RedoTarget = RedoTarget {
  targetPath :: String       -- ^ the relative path to the target
  } deriving (Show, Read)

data Signature =
  NoSignature |        -- ^ for non-existing files
  AnySignature |       -- ^ the wild-card signature which matches any signatures
  Signature String     -- ^ the normal signature containing the MD5 value.
  deriving (Show, Read)

instance Eq Signature where
  (Signature x) == (Signature y) = x == y
  NoSignature == _ = False
  _ == NoSignature = False
  _ == _ = True

data Dependency =
  NonExistingDependency FilePath |      -- ^ non existing dependency issued by `redo-ifcreate`
  ExistingDependency FilePath Signature -- ^ existing dependency
  deriving (Show, Read)

data RedoException =
  NoDoFileExist FilePath |      -- ^ No .do files exist for the target.
  DoExitFailure FilePath Int |  -- ^ .do script fails with the exit code.
  CyclicDependency FilePath |   -- ^ cyclic dependency detected for the target.
  TargetNotGenerated FilePath | -- ^ target is not generated even after redo completes.
  InvalidDependency FilePath    -- ^ redo dependencies are incorrectly recorded for the target.
  deriving (Show, Typeable)

instance Exception RedoException

data RedoSettings = RedoSettings {
  help :: Bool,
  verbose :: Bool,
  xtrace :: Bool,
  inPar   :: Int,
  files   :: [FilePath]
  } deriving (Show, Read)

options :: OptSpec RedoSettings
options = OptSpec {
  progDefaults = RedoSettings {
      help    = False,
      verbose = False,
      xtrace  = False,
      inPar   = 1,
      files   = []
      },
  progOptions = [Option "h" ["help"]
                 "Display usage."
                 $ NoArg $ \s -> Right s { help = True }
                ,
                 Option "v" ["verbose"]
                 "Display more information while working."
                 $ NoArg $ \s -> Right s { verbose = True }
                ,
                 Option "x" ["xtrace"]
                 "Display more information while working."
                 $ NoArg $ \s -> Right s { xtrace = True }
                ],
  progParamDocs = [ ("FILES",   "The files that need processing.") ],
  progParams = \p s -> Right s { files = files s ++ [p] }
  }

-- | This is the directory where dependencies are stored.
configPath :: FilePath
configPath = ".redo"

-- | This is the directory where temporary files are created.
tempPath :: FilePath
tempPath = configPath </> "tmp"

-- | This is the directory where lock files are created.
lockPath :: FilePath
lockPath = configPath </> "lock"

-- | This is the directory where temporary output files are created.
tempOutPath :: FilePath
tempOutPath = tempPath </> "out"

-- | This locks the target.
-- If already locked, this returns False.
lockTarget :: RedoTarget -> IO Bool
lockTarget (RedoTarget file) = do
  locked <- doesFileExist lockFile
  if locked
    then return False
    else createFile lockFile >> return True
  where lockFile = lockPath </> encodePath file

-- | This unlocks the target.
unlockTarget :: RedoTarget -> IO ()
unlockTarget (RedoTarget file)
  = ignoreExceptionM_ . removeFile $ lockPath </> encodePath file

-- | This clears temporary files.
clearGarbage :: IO ()
clearGarbage = do
  ignoreExceptionM_ $ removeDirectoryRecursive lockPath
  ignoreExceptionM_ $ removeDirectoryRecursive tempPath

-- | This returns a temp. file path for the target.
-- This also creates directories for it.
-- This doesn't create the temp. file.
tempOutFilePath :: RedoTarget -> IO FilePath
tempOutFilePath (RedoTarget file) = do
  createDirectoryIfMissing True tempOutPath
  return $ tempOutPath </> encodePath file

-- | Return the signature of a file.
fileSignature :: FilePath -> IO Signature
fileSignature f =
  -- any exception (e.g. file not exists.) causes NoSignature.
  ignoreExceptionM NoSignature (Signature . show . MD5.md5 <$> BL.readFile f)

-- | Add a dependency entry into a file.
addDependency :: FilePath   -- ^ the file which a dependency will be appended to
              -> Dependency -- ^ the dependency
              -> IO ()
addDependency depsFile dep = withFile depsFile AppendMode (`hPrint` dep)

-- | Create a redo target for a file.
-- This calls 'redoTargetFromDir' with the current directory.
redoTarget :: FilePath       -- ^ the file path
           -> IO RedoTarget
redoTarget target = flip redoTargetFromDir target <$> getCurrentDirectory

-- | Create a redo target for a file.
-- This builds a redo target with a relative path for the file.
redoTargetFromDir :: FilePath    -- ^ the base directory, this should be an absolute path.
                  -> FilePath    -- ^ the file path
                  -> RedoTarget
redoTargetFromDir baseDir target =
  if isRelative $ takeDirectory target'
    then RedoTarget target'
    else RedoTarget $ makeRelative' baseDir target'
  where target' = normalise' target

main :: IO ()
main = (intro >>= main' >> outro)
  `catch`
  \e -> case e of
       (NoDoFileExist t) -> die $ "no rule to make " ++ quote t
       (DoExitFailure t err) -> die $ t ++ " failed with exitcode " ++ show err
       (CyclicDependency f) -> die $ "cyclic dependency detected for " ++ f
       (TargetNotGenerated f) -> die $ f ++ " was not generated"
       (InvalidDependency f) -> die $ f ++ " has invalid dependency"
  where
    die err = hPutStrLn stderr err >> exitFailure
    intro = do
      settings <- getOpts options;
      callDepth <- getCallDepth
      when (callDepth == 0) $ do {
        when (help settings) (dumpUsage options >> exitSuccess);
        setEnv "REDO_SH_OPTS" $ unwords [optsToStr settings verbose "-v",
                                         optsToStr settings xtrace "-x"]}
      return $ files settings
    optsToStr settings p o = if p settings then o else ""
    outro = do
      callDepth <- getCallDepth
      when (callDepth == 0) $ hPutStrLn stderr "done"
    main' fs = do
      dir <- getCurrentDirectory
      -- Redo targets are created from the arguments.
      let targets = map (redoTargetFromDir dir) fs
      -- `redo-ifchange` and `redo-ifcreate` are spawned from another `redo` process
      -- with a file path given via an environment variable.
      -- The file is used to store the dependency information.
      maybeDepsPath <- lookupEnv "REDO_DEPS_PATH"
      cmd <- getProgName
      case cmd of
        "redo" -> mapM_ redo targets
        "redo-ifchange" -> do
          -- Signature values of targets will be stored as dependency information.
          sigs <- mapM redo targets
          case maybeDepsPath of
            (Just depsPath) -> do
              let deps = zipWith (\t s -> ExistingDependency (targetPath t) s) targets sigs
              mapM_ (addDependency depsPath) deps
            Nothing -> return ()
        "redo-ifcreate" ->
          case maybeDepsPath of
            (Just depsPath) -> mapM_ (addDependency depsPath . NonExistingDependency . targetPath) targets
            Nothing -> return ()
        _ -> hPrint stderr $ "unknown command: " ++ cmd

-- | Redo is a recursive procedure.  This returns the call depth.
getCallDepth :: IO Int
getCallDepth = ignoreExceptionM 0 (read <$> getEnv "REDO_CALL_DEPTH")

-- | This redo the target.
-- This returns the signature of the target.
-- This may throw
-- * CyclicDependency
-- * TargetNotGenerated
-- * NoDoFileExist
-- * DoExitFailure
redo :: RedoTarget
     -> IO Signature
redo target = do
  callDepth <- getCallDepth
  let indent = replicate callDepth ' '
  hPutStrLn stderr $ indent ++ "redo  " ++ targetFile
  -- Try to lock the target, if False returns, it means that cyclic dependency exists.
  lockAcquired <- lockTarget target
  unless lockAcquired . throwIO $ CyclicDependency targetFile
  unchanged <- upToDate $ ExistingDependency targetFile AnySignature
  finally
    (if unchanged
     then hPutStrLn stderr $ targetFile ++ " is up to date."
     -- Run a do script unless it is up to date.
     else runDo target)
    (unlockTarget target)
  sig <- fileSignature targetFile
  case sig of
    NoSignature -> throwIO $ TargetNotGenerated targetFile
    _ -> return sig
  where targetFile = targetPath target

-- | This recursively visits its dependencies to test whether it is up to date.
upToDate :: Dependency
         -> IO Bool
upToDate (ExistingDependency f oldSig) = do
  newSig <- fileSignature f
  -- first, check if the target has been changed.
  -- print (f, oldSig, newSig)
  if oldSig /= newSig
    then return False
    else do
      target <- redoTarget f
      maybeDeps <- getDependencies target
      case maybeDeps of
        -- Nothing means that no dependency configuration file exist.
        -- This is handled as the target being outdated.
        Nothing -> return False
        Just [] -> return True -- Leaf target
        (Just deps@(ExistingDependency aDo _:_)) -> do
          -- aDo is the do file which generated the target
          -- exDos are do files which have a higher priority than aDo.
          let exDos =  takeWhile (/= aDo) . map snd $ listDoFiles target
          -- checks non-existing dependency for exDos
          and <$> mapM upToDate (map NonExistingDependency exDos ++ deps)
        _ -> throwIO $ InvalidDependency f
upToDate (NonExistingDependency f) = not <$> doesFileExist f

-- | This composes a file path to store dependencies.
depFilePath :: RedoTarget -> FilePath
depFilePath (RedoTarget file) = configPath </> encodePath file

-- | This returns a list of dependencies, i.e. a file path and the signature.
getDependencies :: RedoTarget
                -> IO (Maybe [Dependency])
getDependencies target = ignoreExceptionM Nothing $
  do depLines <- lines <$> readFile (depFilePath target)
     return . Just . map read $ depLines

-- | This finds an appropriate do script and runs it if it exists.
-- This may throw
-- * CyclicDependency
-- * TargetNotGenerated
-- * NoDoFileExist
-- * DoExitFailure
runDo :: RedoTarget
      -> IO ()
runDo target = do
  -- Create a temporary file to store dependencies.
  tmpDeps <- createTempFile tempPath . takeFileName $ targetPath target ++ ".deps"
  -- Create a temporary output file.
  tmpOut <- tempOutFilePath target
  callDepth <- getCallDepth

  exists <- doesFileExist $ targetPath target
  doFiles <- filterM (doesFileExist . snd) (listDoFiles target)
  case doFiles of
    [] -> when (callDepth == 0 || not exists) $
      throwIO . NoDoFileExist $ targetPath target
    (doFile:_) -> catch (executeDo target tmpDeps tmpOut callDepth doFile) $
      \(e :: SomeException) -> do
        ignoreExceptionM_ (removeFile tmpOut)
        ignoreExceptionM_ (removeFile tmpDeps)
        throwIO e

  -- Rename temporary files to actual names, if any exists.
  built <- doesFileExist tmpOut
  when built $ moveFile tmpOut (targetPath target)
  moveFile tmpDeps $ depFilePath target

-- | This handles the list of do files.
-- This adds do files as non-existing dependencies,
-- until it finds and executes the first existing do file.
-- This may throw
-- * CyclicDependency
-- * TargetNotGenerated
-- * NoDoFileExist
-- * DoExitFailure
executeDo :: RedoTarget
          -> FilePath             -- ^ temporary file for storing dependencies
          -> FilePath             -- ^ temporary file for output (redo's $3 argument)
          -> Int                  -- ^ call depth
          -> (String, FilePath)   -- ^ the redo's $2 argument and the do script
          -> IO ()
executeDo target tmpDeps tmpOut callDepth (baseName, doFile) = do
  doFileTarget <- redoTarget doFile
  -- Mark the do file is tracked by redo.
  createFile $ depFilePath doFileTarget
  -- Add the do file itself as the 1st dependency.
  doSig <- fileSignature doFile
  addDependency tmpDeps $ ExistingDependency (targetPath doFileTarget) doSig
  opts <- getEnv "REDO_SH_OPTS"
  -- hPutStrLn stderr $ cmds opts
  ec <- spawnCommand (cmds opts) >>= waitForProcess
  case ec of ExitFailure e -> throwIO $ DoExitFailure (targetPath target) e
             _ -> return ()
 where cmds opts = unwords ["REDO_DEPS_PATH=" ++ quote tmpDeps,
                            "REDO_CALL_DEPTH=" ++ show (callDepth + 1),
                            "REDO_SH_OPTS=" ++ quote opts, "sh -e", opts,
                            quote doFile, quote $ targetPath target,
                            quote baseName, quote tmpOut]

-- | This lists all applicable do files and redo's $2 names.
-- e.g.
-- > listDoFiles "a.b.c"
-- [("a.b.c","a.b.c.do"),("a","default.b.c.do"),("a.b","default.c.do"),("a.b.c","default.do")]
listDoFiles :: RedoTarget -> [(String, FilePath)]
listDoFiles (RedoTarget file) = (file, takeFileName file <.> "do") : defaultDos
  where tokens = splitOn "." (takeFileName file)
        defaultDos = map ((toBaseName *** toFileName) . (`splitAt` tokens)) [1..length tokens]
        toBaseName xs = normalise' $ takeDirectory file </> intercalate "." xs
        toFileName = intercalate "." . ("default":) . (++["do"])
