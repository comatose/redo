module Development.Redo.Config where

import System.Console.ANSI
import System.FilePath
import System.IO

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

printDebug :: String -> IO ()
printDebug s = do
  hSetSGR stderr [SetColor Foreground Vivid Yellow]
  hPutStrLn stderr s
  hSetSGR stderr [Reset]
