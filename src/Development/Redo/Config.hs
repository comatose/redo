module Development.Redo.Config where

import Control.Applicative
import Control.Exception
import System.Console.ANSI
import System.Environment
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Posix.Process
import System.Posix.Semaphore
import System.Posix.Types

configPath :: FilePath
configPath = ".redo"

-- | This is the directory where dependencies are stored.
depsDirPath :: FilePath
depsDirPath = configPath </> "deps"

-- | This is the directory where temporary files are created.
tempDirPath :: FilePath
tempDirPath = configPath </> "tmp"

-- | This is the directory where lock files are created.
lockDirPath :: FilePath
lockDirPath = configPath </> "lock"

-- | This is the directory where temporary output files are created.
tempOutDirPath :: FilePath
tempOutDirPath = tempDirPath </> "out"

printLockPath :: FilePath
printLockPath = configPath </> ".print_lock"

envCallDepth :: String
envCallDepth = "REDO_CALL_DEPTH"

envShellOptions :: String
envShellOptions = "REDO_SH_OPTS"

envDependencyPath :: String
envDependencyPath = "REDO_DEPS_PATH"

envSessionID :: String
envSessionID = "REDO_SESSION_ID"

semaphorePrefix :: String
semaphorePrefix = "/redo_sem_"

globalLockPrefix :: String
globalLockPrefix = "/redo_global_lock_"

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

setSessionID :: IO ()
setSessionID = setEnv envSessionID sessionID

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
printDebug = printWithColor [SetColor Foreground Vivid Yellow]
