{-# LANGUAGE ScopedTypeVariables #-}

module Development.Redo.Config (acquireProcessorToken,
                                callDepth,
                                callerDepsPath,
                                debugMode,
                                depsDirPath,
                                envCallDepth,
                                envDependencyPath,
                                envDebugMode,
                                envParallelBuild,
                                envSessionID,
                                envShellOptions,
                                envTargetHistory,
                                finalize,
                                initialize,
                                parallelBuild,
                                printDebug,
                                printError,
                                printInfo,
                                printSuccess,
                                RedoSettings(..),
                                releaseProcessorToken,
                                sessionID,
                                shellOptions,
                                targetHistory,
                                targetLockPrefix,
                                tempDirPath,
                                tempOutDirPath,
                                withoutProcessorToken,
                                withProcessorToken,
                               ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import System.Console.ANSI
import System.Environment
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Posix.Process
import System.Posix.Semaphore
import System.Posix.Types

-- | This is the directory where all redo configurations are located.
configDirPath :: FilePath
configDirPath = ".redo"

-- | This is the directory where dependency files are located.
depsDirPath :: FilePath
depsDirPath = configDirPath </> "deps"

-- | This is the directory where temporary files are created.
tempDirPath :: FilePath
tempDirPath = configDirPath </> "tmp"

-- | This is the directory where temporary output files are created.
tempOutDirPath :: FilePath
tempOutDirPath = tempDirPath </> "out"

envCallDepth :: String
envCallDepth = "REDO_CALL_DEPTH"

envShellOptions :: String
envShellOptions = "REDO_SH_OPTS"

envDependencyPath :: String
envDependencyPath = "REDO_DEPS_PATH"

envSessionID :: String
envSessionID = "REDO_SESSION_ID"

envDebugMode :: String
envDebugMode = "REDO_DEBUG_MODE"

envTargetHistory :: String
envTargetHistory = "REDO_TARGET_HISTORY"

envParallelBuild :: String
envParallelBuild = "REDO_PARALLEL_BUILD"

semaphorePrefix :: String
semaphorePrefix = "/redo_sem_"

globalLockPrefix :: String
globalLockPrefix = "/redo_global_lock_"

targetLockPrefix :: String
targetLockPrefix = "/redo_target_lock_"

shellOptions :: String
{-# NOINLINE shellOptions #-}
shellOptions = unsafePerformIO $ getEnv envShellOptions

sessionID :: String
{-# NOINLINE sessionID #-}
sessionID = unsafePerformIO $ do
  maybeSID <- lookupEnv envSessionID
  case maybeSID of
    Nothing -> show <$> getProcessID
    (Just sid) -> return sid

createGlobalLock :: IO ()
createGlobalLock = do
  _ <- semOpen (globalLockPrefix ++ sessionID) (OpenSemFlags True True) (CMode 448) 1
  return ()

globalLock :: Semaphore
{-# NOINLINE globalLock #-}
globalLock = unsafePerformIO $
  semOpen (globalLockPrefix ++ sessionID) (OpenSemFlags False False) (CMode 448) 0

destroyGlobalLock :: IO ()
destroyGlobalLock = semUnlink $ globalLockPrefix ++ sessionID

withGlobalLock :: IO a -> IO a
withGlobalLock = bracket_ (semWait globalLock) (semPost globalLock)

printWithColor :: [SGR] -> String -> IO ()
printWithColor color s = withGlobalLock $ bracket_
  (hSetSGR stderr color) (hSetSGR stderr [Reset]) (hPutStrLn stderr s)

printInfo :: String -> IO ()
printInfo = printWithColor [SetColor Foreground Vivid Blue]

printSuccess :: String -> IO ()
printSuccess = printWithColor [SetColor Foreground Vivid Green]

printError :: String -> IO ()
printError = printWithColor [SetColor Foreground Vivid Red]

printDebug :: String -> IO ()
printDebug s = when debugMode $ printWithColor [SetColor Foreground Vivid Yellow] s

data RedoSettings = RedoSettings {
  help :: Bool,
  inPar :: Int,
  files :: [FilePath],
  shellOpts :: String,
  debug :: Bool
  } deriving (Show, Read)

callerDepsPath :: Maybe FilePath
{-# NOINLINE callerDepsPath #-}
callerDepsPath = unsafePerformIO $ lookupEnv envDependencyPath

debugMode :: Bool
{-# NOINLINE debugMode #-}
debugMode = read . unsafePerformIO $ getEnv envDebugMode

parallelBuild :: Int
{-# NOINLINE parallelBuild #-}
parallelBuild = read . unsafePerformIO $ getEnv envParallelBuild

-- | Redo is a recursive procedure.  This returns the call depth.
callDepth :: Int
{-# NOINLINE callDepth #-}
callDepth = unsafePerformIO $ maybe 0 read <$> lookupEnv envCallDepth

targetHistory :: [String]
{-# NOINLINE targetHistory #-}
targetHistory = unsafePerformIO $ maybe [] read <$> lookupEnv envTargetHistory

createProcessorTokens :: Int -> IO ()
createProcessorTokens n = do
  _ <- semOpen semaphoreID (OpenSemFlags True True) (CMode 448) n
  printDebug $ "Semaphore '" ++ semaphoreID ++ "' with " ++ show n ++ " sems created."

destroyProcessorTokens :: IO ()
destroyProcessorTokens = do
  sem <- semOpen semaphoreID (OpenSemFlags False False) (CMode 448) 0
  n <- semGetValue sem
  printDebug $ "Semaphore '" ++ semaphoreID ++ "' with " ++ show n ++ " sems destroyed."
  semUnlink semaphoreID

semaphoreID :: String
semaphoreID = semaphorePrefix ++ sessionID

semaphore :: Semaphore
{-# NOINLINE semaphore #-}
semaphore = unsafePerformIO $ semOpen semaphoreID (OpenSemFlags False False) (CMode 448) 0

acquireProcessorToken :: IO ()
acquireProcessorToken = bracketOnError (semWait semaphore) (const $ semPost semaphore)
  (const $ printProcessorTokens semaphore)

releaseProcessorToken :: IO ()
releaseProcessorToken = bracketOnError (semPost semaphore) (const $ semWait semaphore)
  (const $ printProcessorTokens semaphore)

withProcessorToken :: IO a -> IO a
withProcessorToken = bracket_ acquireProcessorToken releaseProcessorToken

withoutProcessorToken :: IO a -> IO a
withoutProcessorToken = bracket_ releaseProcessorToken acquireProcessorToken

-- exitOnException :: IO a -> IO a
-- exitOnException = handle (\(_ :: SomeException) -> exitFailure)

printProcessorTokens :: Semaphore -> IO ()
printProcessorTokens sem = do
  n <- semGetValue sem
  printDebug $ "Semaphores = " ++ show n

initialize :: RedoSettings -> IO ()
initialize settings = when (callDepth == 0) $ do
  setEnv envSessionID sessionID
  setEnv envShellOptions $ shellOpts settings
  setEnv envDebugMode . show $ debug settings
  setEnv envParallelBuild . show $ inPar settings
  createGlobalLock
  createProcessorTokens (inPar settings - 1)

finalize :: IO ()
finalize =  when (callDepth == 0) $ do
  destroyProcessorTokens
  destroyGlobalLock
