{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Development.Redo (recordDependency,
                         Dependency(..),
                         redo,
                         RedoException(..),
                         RedoTarget,
                         redoTarget,
                         redoTargetFromDir
                        ) where

import qualified Development.Redo.Config as C
import Development.Redo.Util

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import Data.Char
import Data.List
import Data.Typeable
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Semaphore
import System.Posix.Types
import System.Process

-- | A type for redo targets
-- This should be constructed from 'redoTarget' or 'redoTargetFromDir'.
type RedoTarget = FilePath

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
  ExistingDependency RedoTarget Signature -- ^ existing dependency
  deriving (Show, Read)

data RedoException =
  NoDoFileExist RedoTarget |               -- ^ No .do files exist for the target.
  DoExitFailure RedoTarget FilePath Int |  -- ^ .do script fails with the exit code.
  CyclicDependency RedoTarget |            -- ^ cyclic dependency detected for the target.
  TargetNotGenerated RedoTarget FilePath | -- ^ target is not generated even after redo completes.
  InvalidDependency RedoTarget |           -- ^ redo dependencies are incorrectly recorded for the target.
  UnknownRedoCommand String                -- ^ redo is called by an invalid prog. name.
  deriving (Show, Typeable)

instance Exception RedoException

-- | This ensures only one thread to process a target exclusively and
-- detects a cyclic dependency.
--
-- This may throw: * 'CyclicDependency' if a thread tries to acquire
-- the target lock which is already occupied by the thread.
withTargetLock :: RedoTarget -> IO a -> IO a
withTargetLock target io = do
  -- 'targetHistory' contains a list of targets which are being
  -- processed by parent processes.
  when (target `elem` C.targetHistory) . throwIO $ CyclicDependency target

  -- Use a named semaphore to provide a lock for the target.
  sem <- semOpen (C.targetLockPrefix ++ C.sessionID ++ encodePath target)
    (OpenSemFlags True False) (CMode 448) 1
  bracket_ (semThreadWait sem) (semPost sem) io

-- | This returns a temporary file path for the output target.  This
-- does not create the file, but involves creating directories for it.
tempOutFilePath :: RedoTarget -> IO FilePath
tempOutFilePath target = do
  createDirectoryIfMissing True C.tempOutDirPath
  return $ C.tempOutDirPath </> encodePath target

-- | Thes returns the signature of a file.
fileSignature :: FilePath -> IO Signature
fileSignature f =
  -- any exception (e.g. file does not exists.) causes NoSignature.
  ignoreExceptionM NoSignature (Signature . show . MD5.md5 <$> BL.readFile f)

-- | This records a dependency entry in the file.
recordDependency :: FilePath   -- ^ the file which a dependency will be appended to
                 -> Dependency -- ^ the dependency information
                 -> IO ()
recordDependency depsFile dep = withFile depsFile AppendMode (`hPrint` dep)

-- | This creates a redo target for a file.
-- This calls 'redoTargetFromDir' with the current directory.
redoTarget :: FilePath       -- ^ the file path
           -> IO RedoTarget
redoTarget target = flip redoTargetFromDir target <$> getCurrentDirectory

-- | This creates a redo target for a file.
-- This returns 'RedoTarget' created with a relative path for the file.
redoTargetFromDir :: FilePath    -- ^ the base directory, this should be an absolute path.
                  -> FilePath    -- ^ the file path
                  -> RedoTarget
redoTargetFromDir baseDir target =
  if isRelative $ takeDirectory target'
    then target'
    else makeRelative' baseDir target'
  where target' = normalise' target

-- | This performs redo for the target.  This requires a target lock
-- and a processor token to run.  The order of acquisition is
-- important to prevent deadlock. (i.e. a target lock first, and then
-- a processor token) This returns the signature of the target.
--
-- This may throw:
-- * 'CyclicDependency' if a dependency cycle is detected.
-- * 'TargetNotGenerated' if the target is not generated even after performing redo.
-- * 'NoDoFileExist' if the target file does not exists and neither do the proper do-scripts.
-- * 'DoExitFailure' if any do-script exited with failure.
-- * 'InvalidDependency' if dependency information is recorded incorrectly.
-- * 'UnknownRedoCommand' if redo is invoked by unknown commands.
redo :: RedoTarget
     -> IO Signature
redo target = withTargetLock target . C.withProcessorToken $ do
  let indent = replicate C.callDepth ' '
  C.printInfo $ "redo " ++ indent ++ target
  p <- upToDate $ ExistingDependency target AnySignature
  if p
    then C.printInfo $ target ++ " is up to date."
    else runDo target
  fileSignature target

-- | This recursively visits its dependencies to test whether it is up to date.
--
-- This may throw:
-- * 'InvalidDependency' if dependency information is recorded incorrectly.
upToDate :: Dependency
         -> IO Bool
upToDate (ExistingDependency target oldSig) = do
  newSig <- fileSignature target
  -- first, check if the target has been changed.
  if oldSig /= newSig
    then return False
    else do
      maybeDeps <- getDependencies target
      case maybeDeps of
        -- Nothing means that no dependency configuration file exist.
        -- This indicates that the target was not generated by redo.
        Nothing -> return True
        -- Now, it is assured that the target was generated by redo.
        (Just deps@(ExistingDependency aDo _:_)) -> do
          -- aDo is the do file used to generate the target
          -- exDos are do files which have a higher priority than aDo.
          let exDos =  takeWhile (/= aDo) . map snd $ listDoFiles target
          -- call 'upToData' for exDos as well as recorded dependencies.
          and <$> mapM upToDate (map NonExistingDependency exDos ++ deps)
        _ -> throwIO $ InvalidDependency target
upToDate (NonExistingDependency target) = not <$> doesFileExist target

-- | This composes a file path to store dependencies.
depFilePath :: RedoTarget -> FilePath
depFilePath target = C.depsDirPath </> encodePath target

-- | This returns a list of dependencies.
getDependencies :: RedoTarget
                -> IO (Maybe [Dependency])
getDependencies target = ignoreExceptionM Nothing $
  do depLines <- lines <$> readFile (depFilePath target)
     return . Just . map read $ depLines

-- | This finds an appropriate do script and runs it if it exists.
--
-- This may throw:
-- * 'CyclicDependency' if a dependency cycle is detected.
-- * 'TargetNotGenerated' if the target is not generated even after performing redo.
-- * 'NoDoFileExist' if the target file does not exists and neither do the proper do-scripts.
-- * 'DoExitFailure' if any do-script exited with failure.
-- * 'InvalidDependency' if dependency information is recorded incorrectly.
-- * 'UnknownRedoCommand' if redo is invoked by unknown commands.
runDo :: RedoTarget
      -> IO ()
runDo target = do
  doFiles <- filterM (doesFileExist . snd) (listDoFiles target)
  when (null doFiles) $ throwIO . NoDoFileExist $ target

  -- Create a temporary file to store dependencies.
  tmpDeps <- createTempFile C.tempDirPath . takeFileName $ target ++ ".deps"
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
--
-- This may throw:
-- * 'CyclicDependency' if a dependency cycle is detected.
-- * 'TargetNotGenerated' if the target is not generated even after performing redo.
-- * 'NoDoFileExist' if the target file does not exists and neither do the proper do-scripts.
-- * 'DoExitFailure' if any do-script exited with failure.
-- * 'InvalidDependency' if dependency information is recorded incorrectly.
-- * 'UnknownRedoCommand' if redo is invoked by unknown commands.
executeDo :: RedoTarget
          -> FilePath             -- ^ temporary file for storing dependencies
          -> FilePath             -- ^ temporary file for output (redo's $3 argument)
          -> (String, FilePath)   -- ^ the redo's $2 argument and the do script
          -> IO ()
executeDo target tmpDeps tmpOut (baseName, doFile) = do
  -- Add the do file itself as the 1st dependency.
  doSig <- fileSignature doFile
  recordDependency tmpDeps $ ExistingDependency doFile doSig
  ec <- runCmd >>= waitForProcess
  case ec of
    ExitFailure e -> throwIO $ DoExitFailure target doFile e
    _ -> do built <- doesFileExist tmpOut
            unless built $ throwIO (TargetNotGenerated target doFile)
 where
   runCmd = do
     let args = map quote [doFile, target, baseName, tmpOut]
     exe <- getExecutor doFile
     C.printDebug . unwords $ exe : args
     oldEnv <- getEnvironment
     (_, _, _, h) <- createProcess $ (shell . unwords $ exe : args)
       {env = Just $ oldEnv ++ [(C.envDependencyPath, tmpDeps),
                      (C.envCallDepth, show (C.callDepth + 1)),
                      (C.envShellOptions, C.shellOptions),
                      (C.envSessionID, C.sessionID),
                      (C.envDebugMode, show C.debugMode),
                      (C.envTargetHistory, show (target : C.targetHistory))]}
     return h
   getExecutor dofile = do
     exe <- ignoreExceptionM "sh" $ do
       ('#':'!':exe) <- withFile dofile ReadMode hGetLine
       return $ strip exe
     if "sh" `isSuffixOf` exe
        -- if the executable is ends with sh, it will run with shell options.
       then return $ exe ++ C.shellOptions
       else return exe
   strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

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
