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
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

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
unlockTarget (RedoTarget file) = handle
  (\(_ :: SomeException) -> return ())
  (removeFile $ lockPath </> encodePath file)

-- | This clears all of the target locks.
clearGarbage :: IO ()
clearGarbage = handle
  (\(_ :: SomeException) -> return ())
  (removeDirectoryRecursive lockPath >>
   removeDirectoryRecursive tempPath)

-- | This returns a temp. file path for the target.
-- This also creates directories for it.
-- This doesn't create the temp. file.
tempOutFilePath :: RedoTarget -> IO FilePath
tempOutFilePath target = do
  createDirectoryIfMissing True $ takeDirectory filePath
  return filePath
  where filePath = tempOutPath </> encodePath (targetPath target)

data RedoException =
  NoDoFileExist FilePath |     -- ^ No .do files exist for the target.
  DoExitFailure FilePath Int | -- ^ .do script fails with the exit code.
  CyclicDependency FilePath |  -- ^ cyclic dependency detected for the target.
  TargetNotGenerated FilePath
  deriving (Show, Typeable)

instance Exception RedoException

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

-- | Return the signature of a file.
fileSignature :: FilePath -> IO Signature
fileSignature f = handle
  -- any exception (e.g. file not exists.) causes NoSignature.
  (\(_ :: SomeException) -> return NoSignature)
  (Signature . show . MD5.md5 <$> BL.readFile f)

-- | Add a dependency entry into a file.
addDependency :: FilePath   -- ^ the file which a dependency will be appended to
              -> Dependency -- ^ the dependency
              -> IO ()
addDependency depsFile dep = withFile depsFile AppendMode (`hPrint` dep)

-- | A type for redo targets
-- This should be constructed from 'redoTarget' or 'redoTargetFromDir'.
newtype RedoTarget = RedoTarget {
  targetPath :: String       -- ^ the relative path to the target
  } deriving (Show, Read)

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
main = (initialize >> main')
  `catch`
  \e -> case e of
    (NoDoFileExist t) -> hPutStrLn stderr $ "no rule to make " ++ quote t
    (DoExitFailure t err) -> hPutStrLn stderr $ t ++ " failed with exitcode " ++ show err
    (CyclicDependency f) -> hPutStrLn stderr $ "cyclic dependency detected for " ++ f
    (TargetNotGenerated f) -> hPutStrLn stderr $ f ++ " was not generated"
  where
    initialize = do
      callDepth <- getCallDepth
      when (callDepth == 0) clearGarbage
    main' = do
      dir <- getCurrentDirectory
      cmd <- getProgName
      -- Redo targets are created from the arguments.
      targets <- map (redoTargetFromDir dir) <$> getArgs
      -- `redo-ifchange` and `redo-ifcreate` are spawned from another `redo` process
      -- with a file path given via an environment variable.
      -- The file is used to store the dependency information.
      maybeDepsPath <- getDepsPath
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

-- | This returns a file path for storing dependencies,
-- if the current process is spawned during the execution of other do scripts.
getDepsPath :: IO (Maybe String)
getDepsPath = lookupEnv "REDO_DEPS_PATH"

-- | Redo is a recursive procedure.  This returns the call depth.
getCallDepth :: IO Int
getCallDepth = handle
  (\(_ :: SomeException) -> return 0)
  (read <$> getEnv "REDO_CALL_DEPTH")

-- | This redo the target.
-- This returns the signature of the target.
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
  if unchanged
    then hPutStrLn stderr $ targetFile ++ " is up to date."
    -- Run a do script unless it is up to date.
    else runDo target
         `catch`
         \e -> case e of
           (NoDoFileExist t) -> hPutStrLn stderr $ "no rule to make " ++ quote t
           (DoExitFailure t err) -> hPutStrLn stderr $ t ++ " failed with exitcode " ++ show err
           other -> throwIO other
  unlockTarget target
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
      -- print (target, maybeDeps)
      case maybeDeps of
        -- Nothing means that no dependency configuration file exist.
        -- This is handled as the target being outdated.
        Nothing -> return False
        (Just deps) -> if null deps
                       then return True  -- Leaf target
                       else and <$> mapM upToDate deps
upToDate (NonExistingDependency f) = not <$> doesFileExist f

-- | This composes a file path to store dependencies.
depFilePath :: RedoTarget -> FilePath
depFilePath target = configPath </> encodePath (targetPath target)

-- | This returns a list of dependencies, i.e. a file path and the signature.
getDependencies :: RedoTarget
                -> IO (Maybe [Dependency])
getDependencies target = handle
  (\(_ :: SomeException) -> return Nothing)
  (do depLines <- lines <$> readFile (depFilePath target)
      return . Just . map read $ depLines)

-- | This finds an appropriate do script and runs it if it exists.
runDo :: RedoTarget -> IO ()
runDo target = do
  doFiles <- filterM (doesFileExist . snd) (listDoFiles target)
  if null doFiles
    then handleNoDo
    else executeDo target $ head doFiles
  where handleNoDo = do
          callDepth <- getCallDepth
          exists <- doesFileExist $ targetPath target
          if callDepth == 0 || not exists
            then throwIO . NoDoFileExist $ targetPath target
            else createFile $ depFilePath target

-- | This executes the do script.
executeDo :: RedoTarget
          -> (String, FilePath)   -- ^ the redo's $2 argument and the do script
          -> IO ()
executeDo target (baseName, doFile) = do
  doFileTarget <- redoTarget doFile
  -- Mark the do file is tracked by redo.
  createFile $ depFilePath doFileTarget
  -- Create a temporary file to store dependencies.
  tmpDeps <- createTempFile tempPath . takeFileName $ targetPath target ++ ".deps"
  tmpOut <- tempOutFilePath target
  -- Add the do file itself as a dependency.
  doSig <- fileSignature doFile
  addDependency tmpDeps $ ExistingDependency (targetPath doFileTarget) doSig
  callDepth <- getCallDepth
  -- print $ cmds tmpDeps (callDepth + 1) tmpOut
  ph <- spawnCommand $ cmds tmpDeps (callDepth + 1) tmpOut
  ec <- waitForProcess ph
  case ec of
    ExitSuccess -> do
      -- Rename temporary files to actual names, if any exists.
      built <- doesFileExist tmpOut
      when built $ moveFile tmpOut (targetPath target)
      moveFile tmpDeps $ depFilePath target
    ExitFailure err -> do
      -- Remove temporary files.
      removeFile tmpOut
      removeFile tmpDeps
      throwIO $ DoExitFailure (targetPath target) err

  where cmds tmpDeps callDepth tmpOut
          = unwords ["REDO_DEPS_PATH=" ++ quote tmpDeps,
                     "REDO_CALL_DEPTH=" ++ show callDepth,
                     "sh -e", quote doFile, quote $ targetPath target,
                     quote baseName, quote tmpOut]

-- | This lists all applicable do files and redo's $2 names.
-- e.g.
-- > listDoFiles "a.b.c"
-- [("a.b.c","a.b.c.do"),("a","default.b.c.do"),("a.b","default.c.do"),("a.b.c","default.do")]
listDoFiles :: RedoTarget -> [(String, FilePath)]
listDoFiles (RedoTarget target) = (target, takeFileName target <.> "do") : defaultDos
  where tokens = splitOn "." (takeFileName target)
        defaultDos = map ((toBaseName *** toFileName) . (`splitAt` tokens)) [1..length tokens]
        toBaseName xs = normalise' $ takeDirectory target </> intercalate "." xs
        toFileName = intercalate "." . ("default":) . (++["do"])
