module Development.Redo.Config where

import System.FilePath

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
