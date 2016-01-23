{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Development.Redo.Config
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
import System.Console.ANSI
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

-- | A type for redo targets
-- This should be constructed from 'redoTarget' or 'redoTargetFromDir'.
type RedoTarget = FilePath
-- newtype RedoTarget = RedoTarget {
--   targetPath :: String       -- ^ the relative path to the target
--   } deriving (Show, Read)

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
  InvalidDependency FilePath |  -- ^ redo dependencies are incorrectly recorded for the target.
  UnknownRedoCommand String     -- ^ redo is called by an invalid prog. name.
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
  progParams = \p s -> Right s { files = files s ++ [p] }}

printInfo :: String -> IO ()
printInfo s = do
  hSetSGR stderr [SetColor Foreground Vivid Blue]
  hPutStrLn stderr s
  hSetSGR stderr [Reset]

printSuccess :: String -> IO ()
printSuccess s = do
  hSetSGR stderr [SetColor Foreground Vivid Green]
  hPutStrLn stderr s
  hSetSGR stderr [Reset]

printError :: String -> IO ()
printError s = do
  hSetSGR stderr [SetColor Foreground Vivid Red]
  hPutStrLn stderr s
  hSetSGR stderr [Reset]

-- | This locks the target.
-- If already locked, this returns False.
lockTarget :: RedoTarget -> IO Bool
lockTarget target = do
  locked <- doesFileExist lockFile
  if locked
    then return False
    else createFile lockFile >> return True
  where lockFile = lockPath </> encodePath target

-- | This unlocks the target.
unlockTarget :: RedoTarget -> IO ()
unlockTarget target
  = ignoreExceptionM_ . removeFile $ lockPath </> encodePath target

-- | This returns a temp. file path for the target.
-- This also creates directories for it.
-- This doesn't create the temp. file.
tempOutFilePath :: RedoTarget -> IO FilePath
tempOutFilePath target = do
  createDirectoryIfMissing True tempOutPath
  return $ tempOutPath </> encodePath target

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
    then target'
    else makeRelative' baseDir target'
  where target' = normalise' target

main :: IO ()
main = (intro >>= main')
  `catch`
  \e -> case e of
       (NoDoFileExist t) -> die $ "no rule to make " ++ quote t
       (DoExitFailure t err) -> die $ t ++ " failed with exitcode " ++ show err
       (CyclicDependency f) -> die $ "cyclic dependency detected for " ++ f
       (TargetNotGenerated f) -> die $ f ++ " was not generated"
       (InvalidDependency f) -> die $ f ++ " has invalid dependency"
       (UnknownRedoCommand cmd) -> die $ "unknown command: " ++ cmd
  where
    die err = printError err >> exitFailure
    intro = do
      settings <- getOpts options;
      callDepth <- getCallDepth
      when (callDepth == 0) $ do {
        when (help settings) (dumpUsage options >> exitSuccess);
        setEnv "REDO_SH_OPTS" $ unwords [optsToStr settings verbose "-v",
                                         optsToStr settings xtrace "-x"];
        }
      return $ files settings
    optsToStr settings p o = if p settings then o else ""
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
              let deps = zipWith ExistingDependency targets sigs
              mapM_ (addDependency depsPath) deps
            Nothing -> return ()
        "redo-ifcreate" ->
          case maybeDepsPath of
            (Just depsPath) -> mapM_ (addDependency depsPath . NonExistingDependency) targets
            Nothing -> return ()
        _ -> throwIO $ UnknownRedoCommand cmd
      callDepth <- getCallDepth
      when (callDepth == 0) $ printSuccess "done"

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
-- * InvalidDependency
-- * UnknownRedoCommand
redo :: RedoTarget
     -> IO Signature
redo target = do
  callDepth <- getCallDepth
  let indent = replicate callDepth ' '
  printInfo $ "redo " ++ indent ++targetFile
  -- Try to lock the target, if False returns, it means that cyclic dependency exists.
  lockAcquired <- lockTarget target
  unless lockAcquired . throwIO $ CyclicDependency targetFile
  unchanged <- upToDate $ ExistingDependency targetFile AnySignature
  finally
    (if unchanged
     then printInfo $ targetFile ++ " is up to date."
     -- Run a do script unless it is up to date.
     else runDo target)
    (unlockTarget target)
  sig <- fileSignature targetFile
  case sig of
    NoSignature -> throwIO $ TargetNotGenerated targetFile
    _ -> return sig
  where targetFile = target

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
        -- This is not generated by redo.
        Nothing -> return True
        -- Now, it is assured that the target was generated by redo.
        (Just deps@(ExistingDependency aDo _:_)) -> do
          -- aDo is the do file used to generate the target
          -- exDos are do files which have a higher priority than aDo.
          let exDos =  takeWhile (/= aDo) . map snd $ listDoFiles target
          -- checks non-existing dependency for exDos
          and <$> mapM upToDate (map NonExistingDependency exDos ++ deps)
        _ -> throwIO $ InvalidDependency f
upToDate (NonExistingDependency f) = not <$> doesFileExist f

-- | This composes a file path to store dependencies.
depFilePath :: RedoTarget -> FilePath
depFilePath target = configPath </> encodePath target

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
-- * InvalidDependency
-- * UnknownRedoCommand
runDo :: RedoTarget
      -> IO ()
runDo target = do
  doFiles <- filterM (doesFileExist . snd) (listDoFiles target)
  when (null doFiles) $ throwIO . NoDoFileExist $ target

  -- Create a temporary file to store dependencies.
  tmpDeps <- createTempFile tempPath . takeFileName $ target
  -- Create a temporary output file.
  tmpOut <- tempOutFilePath target

  catch
    (executeDo target tmpDeps tmpOut (head doFiles)) $
    \(e :: SomeException) -> do
      ignoreExceptionM_ (removeFile tmpOut)
      ignoreExceptionM_ (removeFile tmpDeps)
      throwIO e

  -- Rename temporary files to actual names.
  ignoreExceptionM_ $ moveFile tmpOut target
  ignoreExceptionM_ $ moveFile tmpDeps (depFilePath target)

-- | This executes the do file.
-- This may throw
-- * CyclicDependency
-- * TargetNotGenerated
-- * NoDoFileExist
-- * DoExitFailure
-- * InvalidDependency
-- * UnknownRedoCommand
executeDo :: RedoTarget
          -> FilePath             -- ^ temporary file for storing dependencies
          -> FilePath             -- ^ temporary file for output (redo's $3 argument)
          -> (String, FilePath)   -- ^ the redo's $2 argument and the do script
          -> IO ()
executeDo target tmpDeps tmpOut (baseName, doFile) = do
  callDepth <- getCallDepth
  -- Add the do file itself as the 1st dependency.
  doSig <- fileSignature doFile
  addDependency tmpDeps $ ExistingDependency doFile doSig
  opts <- getEnv "REDO_SH_OPTS"
  ec <- spawnCommand (cmds callDepth opts) >>= waitForProcess
  case ec of ExitFailure e -> throwIO $ DoExitFailure target e
             _ -> return ()
 where cmds callDepth opts =
         unwords ["REDO_DEPS_PATH=" ++ quote tmpDeps,
                  "REDO_CALL_DEPTH=" ++ show (callDepth + 1),
                  "REDO_SH_OPTS=" ++ quote opts, "sh -e", opts,
                  quote doFile, quote target,
                  quote baseName, quote tmpOut]

-- | This lists all applicable do files and redo's $2 names.
-- e.g.
-- > listDoFiles "a.b.c"
-- [("a.b.c","a.b.c.do"),("a","default.b.c.do"),("a.b","default.c.do"),("a.b.c","default.do")]
listDoFiles :: RedoTarget -> [(String, FilePath)]
listDoFiles target = (target, takeFileName target <.> "do") : defaultDos
  where tokens = splitOn "." (takeFileName target)
        defaultDos = map ((toBaseName *** toFileName) . (`splitAt` tokens)) [1..length tokens]
        toBaseName xs = normalise' $ takeDirectory target </> intercalate "." xs
        toFileName = intercalate "." . ("default":) . (++["do"])
