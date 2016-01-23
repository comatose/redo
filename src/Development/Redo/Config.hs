module Development.Redo.Config where

import System.Console.ANSI
import System.FilePath
import System.IO
import System.FileLock

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

envSemaphoreID :: String
envSemaphoreID = "REDO_SEM_ID"

semaphorePrefix :: String
semaphorePrefix = "/redo_sem_"

printWithColor :: [SGR] -> String -> IO ()
printWithColor _ = hPutStrLn stderr
-- printWithColor color s = withFileLock printLockPath Exclusive $ const go
--   where go = do hSetSGR stderr color
--                 hPutStrLn stderr s
--                 hSetSGR stderr [Reset]

printInfo :: String -> IO ()
printInfo = printWithColor [SetColor Foreground Vivid Blue]

printSuccess :: String -> IO ()
printSuccess = printWithColor [SetColor Foreground Vivid Green]

printError :: String -> IO ()
printError = printWithColor [SetColor Foreground Vivid Red]

printDebug :: String -> IO ()
printDebug = printWithColor [SetColor Foreground Vivid Yellow]
