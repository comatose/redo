module Development.Redo.Config where

import Development.Redo.Util

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

configDirPath :: FilePath
configDirPath = ".redo"

-- | This is the directory where dependencies are stored.
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
withGlobalLock = bracket_ (semThreadWait globalLock) (semPost globalLock)

printWithColor :: [SGR] -> String -> IO ()
printWithColor color s = withGlobalLock go
  where go = do hSetSGR stderr color
                hPutStrLn stderr s
                hSetSGR stderr [Reset]

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
  verbose :: Bool,
  xtrace :: Bool,
  inPar :: Int,
  files :: [FilePath],
  debug :: Bool
  } deriving (Show, Read)

configSession :: RedoSettings -> IO ()
configSession settings = do
  setEnv envSessionID sessionID
  setEnv envShellOptions $ unwords [optsToStr verbose "-v",
                                    optsToStr xtrace "-x"];
  setEnv envDebugMode . show $ debug settings
 where optsToStr p o = if p settings then o else ""

callerDepsPath :: Maybe FilePath
{-# NOINLINE callerDepsPath #-}
callerDepsPath = unsafePerformIO $ lookupEnv envDependencyPath

debugMode :: Bool
{-# NOINLINE debugMode #-}
debugMode = read . unsafePerformIO $ getEnv envDebugMode

targetHistory :: [String]
{-# NOINLINE targetHistory #-}
-- targetHistory = unsafePerformIO $ read <$> getEnv envTargetHistory
targetHistory = unsafePerformIO $ ignoreExceptionM [] (read <$> getEnv envTargetHistory)

createProcessorTokens :: Int -> IO ()
createProcessorTokens n = do
  let sid = semaphorePrefix ++ sessionID
  _ <- semOpen sid (OpenSemFlags True True) (CMode 448) n
  printDebug $ "Semaphore '" ++ sid ++ "' with " ++ show n ++ " sems created."

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

withProcessorToken :: IO a -> IO a
withProcessorToken = bracket_ (semWait semaphore >> printProcessorTokens semaphore)
  (semPost semaphore >> printProcessorTokens semaphore)

withoutProcessorToken :: IO a -> IO a
withoutProcessorToken = bracket_ (semPost semaphore >> printProcessorTokens semaphore)
  (semWait semaphore >> printProcessorTokens semaphore)

printProcessorTokens :: Semaphore -> IO ()
printProcessorTokens sem = do
  n <- semGetValue sem
  printDebug $ "Semaphores = " ++ show n
