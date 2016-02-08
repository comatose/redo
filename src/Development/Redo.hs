{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Development.Redo (redo,
                         redoIfChange,
                         redoIfCreate,
                         redoIfTouch,
                         redoStatus,
                         RedoException(..),
                         relayRedo,
                         withRedo,
                         C.callDepth,
                         C.finalize,
                         C.initialize,
                         C.RedoSettings(..),
                         C.printDebug,
                         C.printError,
                         C.printInfo,
                         C.printSuccess
                        ) where

import qualified Development.Redo.Config as C
import Development.Redo.Util

import Control.Arrow
import Control.Applicative
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
import System.Posix.Files
import System.Posix.Semaphore
import System.Posix.Types
import System.Process

-- | A type for redo targets
-- This should be constructed from 'redoTarget' or 'redoTargetFromDir'.
type RedoTarget = FilePath

data RedoTargetStatus = UpToDate Bool | Outdated | Conflicted deriving Show

data Signature =
  NoSignature |        -- ^ for non-existing files
  AnySignature |       -- ^ the wild-card signature which matches any signatures
  MD5Sum String |      -- ^ the normal signature containing the MD5 value.
  TimeStamp String     -- ^ the normal signature containing the time stamp.
  deriving (Show, Read)

instance Eq Signature where
  (MD5Sum x) == (MD5Sum y) = x == y
  (TimeStamp x) == (TimeStamp y) = x == y
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
  UnknownRedoCommand String |              -- ^ redo is called by an invalid prog. name.
  TargetAlreadyExist RedoTarget
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
  if C.parallelBuild > 1
    then bracket acquireTargetLock semPost $ const io
    else io
 where acquireTargetLock = do
         -- Use a named semaphore to provide a lock for the target.
         sem <- semOpen (C.targetLockPrefix ++ C.sessionID ++ encodePath target)
           (OpenSemFlags True False) (CMode 448) 1
         p <- semTryWait sem
         unless p . C.withoutProcessorToken $ semWait sem
         return sem

-- | This returns a temporary file path for the output target.  This
-- does not create the file, but involves creating directories for it.
tempOutFilePath :: RedoTarget -> IO FilePath
tempOutFilePath target = do
  createDirectoryIfMissing True C.tempOutDirPath
  return $ C.tempOutDirPath </> encodePath target

-- | Thes returns the signature of a file.
fileMD5 :: FilePath -> IO Signature
fileMD5 f =
  -- any exception (e.g. file does not exists.) causes NoSignature.
  ignoreIOExceptionM NoSignature (MD5Sum . show . MD5.md5 <$> BL.readFile f)

-- | Thes returns the signature of a file.
fileStamp :: FilePath -> IO Signature
fileStamp f =
  -- any exception (e.g. file does not exists.) causes NoSignature.
  ignoreIOExceptionM NoSignature (TimeStamp . show . modificationTimeHiRes <$> getFileStatus f)

-- | This records a dependency entry in the file.
recordDependency :: FilePath   -- ^ the file which a dependency will be appended to
                 -> Dependency -- ^ the dependency information
                 -> IO ()
recordDependency depsFile dep = withFile depsFile AppendMode (`hPrint` dep)

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

withRedo :: C.RedoSettings -> IO () -> IO ()
withRedo settings = bracket_ (C.initialize settings) C.finalize

-- | This concurrently performs redo for multiple targets.
-- This may throw:
-- * 'CyclicDependency' if a dependency cycle is detected.
-- * 'TargetNotGenerated' if the target is not generated even after performing redo.
-- * 'NoDoFileExist' if the target file does not exists and neither do the proper do-scripts.
-- * 'DoExitFailure' if any do-script exited with failure.
-- * 'InvalidDependency' if dependency information is recorded incorrectly.
-- * 'UnknownRedoCommand' if redo is invoked by unknown commands.
redoGeneric :: (FilePath -> IO Signature)
            -> [FilePath]
            -> IO ()
redoGeneric sigG fs = do
  dir <- getCurrentDirectory
  -- Redo targets are created from the arguments.
  let targets = map (redoTargetFromDir dir) fs
  -- release a process token for sub-processes
  if C.parallelBuild == 1
    then mapM_ relayRedo targets >> collectResult targets []
    else C.withoutProcessorToken $
    -- make all interruptible except joining spawned sub-processes.
    mask $ \restore -> do
      ps <- restore $ parRedo targets
      rs <- mapM joinChild ps
      restore $ collectResult targets rs
 where parRedo = parRedo' []
       parRedo' (ps) (t:ts) = do
         -- if redo processes which have started earlier exit with error, stop throwing 'DoExitFailure'.
         mapM_ checkInterrupted ps
         mask $ \restore -> do
           C.acquireProcessorToken
           -- once a child is normaly spawned, it will release the token eventually, see "RelayRedo.hs".
           p <- restore (spawnChild t) `onException` C.releaseProcessorToken
           restore (parRedo' (p:ps) ts) `onException` stopChild p
       parRedo' ps _ = return $ reverse ps

       spawnChild t = spawnProcess "relay-redo" [t]
       joinChild = waitForProcess
       stopChild = waitForProcess
       checkInterrupted p = do
         r <- getProcessExitCode p
         case r of
           Just (ExitFailure n) -> throwIO $ DoExitFailure "" "" n
           _ -> return ()

       collectResult targets [] = case C.callerDepsPath of
         (Just depsPath) -> mapM_ (\f -> sigG f >>= recordDependency depsPath . ExistingDependency f) targets
         Nothing -> return ()
       collectResult targets (ExitSuccess:rest) = collectResult targets rest
       collectResult _ (ExitFailure n : _) = throwIO $ DoExitFailure "" "" n

redo :: [FilePath] -> IO ()
redo = redoGeneric fileMD5

-- |
-- This may throw:
-- * 'CyclicDependency' if a dependency cycle is detected.
-- * 'TargetNotGenerated' if the target is not generated even after performing redo.
-- * 'NoDoFileExist' if the target file does not exists and neither do the proper do-scripts.
-- * 'DoExitFailure' if any do-script exited with failure.
-- * 'InvalidDependency' if dependency information is recorded incorrectly.
-- * 'UnknownRedoCommand' if redo is invoked by unknown commands.
redoIfChange :: [FilePath] -> IO ()
redoIfChange = redo

redoIfCreate :: [FilePath] -> IO ()
redoIfCreate fs = do
  dir <- getCurrentDirectory
  -- Redo targets are created from the arguments.
  let targets = map (redoTargetFromDir dir) fs
  case C.callerDepsPath of
    (Just depsPath) -> mapM_ (recordDependency depsPath . NonExistingDependency) targets
    Nothing -> return ()

-- |
-- This may throw:
-- * 'CyclicDependency' if a dependency cycle is detected.
-- * 'TargetNotGenerated' if the target is not generated even after performing redo.
-- * 'NoDoFileExist' if the target file does not exists and neither do the proper do-scripts.
-- * 'DoExitFailure' if any do-script exited with failure.
-- * 'InvalidDependency' if dependency information is recorded incorrectly.
-- * 'UnknownRedoCommand' if redo is invoked by unknown commands.
redoIfTouch :: [FilePath] -> IO ()
redoIfTouch = redoGeneric fileStamp

-- |
-- This may throw:
-- * 'CyclicDependency' if a dependency cycle is detected.
-- * 'TargetNotGenerated' if the target is not generated even after performing redo.
-- * 'NoDoFileExist' if the target file does not exists and neither do the proper do-scripts.
-- * 'DoExitFailure' if any do-script exited with failure.
-- * 'InvalidDependency' if dependency information is recorded incorrectly.
-- * 'UnknownRedoCommand' if redo is invoked by unknown commands.
relayRedo :: RedoTarget
          -> IO ()
relayRedo target = withTargetLock target $ do
  let indent = replicate C.callDepth ' '
  C.printDebug $ "visit " ++ indent ++ target
  st <- targetStatus target
  case st of
    Outdated -> runDo target
    UpToDate False -> C.printDebug $ target ++ " is up to date."
    UpToDate True -> C.printDebug $ target ++ " is native."
    Conflicted -> throwIO $ TargetAlreadyExist target

redoStatus :: [FilePath] -> IO ()
redoStatus fs = do
  dir <- getCurrentDirectory
  -- Redo targets are created from the arguments.
  let targets = map (redoTargetFromDir dir) fs
  mapM_ (targetStatus >=> print) targets

-- | This recursively visits its dependencies to test whether it is up to date.
--
-- This may throw:
-- * 'InvalidDependency' if dependency information is recorded incorrectly.
targetStatus :: RedoTarget
             -> IO RedoTargetStatus
targetStatus target = do
  exist <- doesFileExist target
  let doFiles = listDoFiles target
  doable <- or <$> mapM (doesFileExist . snd) doFiles
  maybeDeps <- getDependencies target
  case (exist, doable, maybeDeps) of
    (False, _, _) -> return Outdated
    (True, False, Nothing) -> return $ UpToDate True
    (True, True, Nothing) -> return Conflicted
    (True, _, Just deps@(ExistingDependency aDo _:_)) -> do
      -- aDo is the do file used to generate the target
      -- exDos are do files which have a higher priority than aDo.
      let exDos =  takeWhile (/= aDo) $ map snd doFiles
      -- get status for exDos as well as recorded dependencies.
      collect (map NonExistingDependency exDos ++ deps)
    _ -> throwIO $ InvalidDependency target
 where
   collect [] = return $ UpToDate False
   collect (d:ds) = do
     st <- targetStatusDown d
     case st of
       (UpToDate _) -> collect ds
       _ -> return st

targetStatusDown :: Dependency -> IO RedoTargetStatus
targetStatusDown (NonExistingDependency target) = do
  exist <- doesFileExist target
  if exist then return Outdated else return (UpToDate False)
targetStatusDown (ExistingDependency target oldSig) = do
  newSig <- getNewSig
  if oldSig /= newSig
    then return Outdated
    else targetStatus target
 where getNewSig = case oldSig of
         MD5Sum _ -> fileMD5 target
         TimeStamp _ -> fileStamp target
         _ -> return NoSignature

-- | This composes a file path to store dependencies.
depFilePath :: RedoTarget -> FilePath
depFilePath target = C.depsDirPath </> encodePath target

-- | This returns a list of dependencies.
getDependencies :: RedoTarget
                -> IO (Maybe [Dependency])
getDependencies target = ignoreIOExceptionM Nothing $
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
  let indent = replicate C.callDepth ' '
  C.printInfo $ "redo " ++ indent ++ target

  doFiles <- filterM (doesFileExist . snd) (listDoFiles target)
  when (null doFiles) $ throwIO . NoDoFileExist $ target

  -- Create a temporary file to store dependencies.
  bracketOnError (createTempFile C.tempDirPath . takeFileName $ target ++ ".deps")
    (ignoreIOExceptionM_ . removeFile) $
    \tmpDeps ->
      -- Create a temporary output file.
      bracketOnError (tempOutFilePath target) (ignoreIOExceptionM_ . removeFile) $
        \tmpOut -> do
          executeDo target tmpDeps tmpOut (head doFiles)
          -- Rename temporary files to actual names.
          moveFile tmpOut target
          moveFile tmpDeps (depFilePath target)

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
  doSig <- fileMD5 doFile
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
       {env = Just $ oldEnv ++ [(C.envCallDepth, show (C.callDepth + 1)),
                                (C.envShellOptions, C.shellOptions),
                                (C.envSessionID, C.sessionID),
                                (C.envDebugMode, show C.debugMode),
                                (C.envParallelBuild, show C.parallelBuild),
                                (C.envDependencyPath, tmpDeps),
                                (C.envTargetHistory, show (target : C.targetHistory))]}
     return h
   getExecutor dofile = do
     line <- ignoreIOExceptionM "" $ withFile dofile ReadMode hGetLine
     let exe = case line of
           ('#':'!':e) -> strip e
           _ -> "sh"
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
