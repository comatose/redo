{-# LANGUAGE CPP #-}

import Control.Monad (forM_, when)
import Distribution.Simple
import Distribution.Simple.InstallDirs (InstallDirs(..), fromPathTemplate, toPathTemplate)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup (ConfigFlags(..), fromFlag, fromFlagOrDefault)
import System.FilePath ((</>))
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.Directory (copyFile)
#else
import System.Directory (doesFileExist, removeFile)
import System.Posix.Files (createSymbolicLink)
#endif

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
  -- Uses a postCopy hook because postInst is not working.
  postCopy = postInstall}
 where postInstall _ _ _ buildInfo = do
         let dirs = configInstallDirs $ configFlags buildInfo
             dir = (fromPathTemplate . fromFlag $ prefix dirs) </> (fromPathTemplate . fromFlagOrDefault (toPathTemplate "bin") $ bindir dirs)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
         mapM_ (copyFile "redo.exe") $ map (dir </>) ["redo-ifchange", "redo-ifcreate", "redo-stamp"]
#else
         makeSymlinks "redo" $ map (dir </>) ["redo-ifchange", "redo-ifcreate", "redo-stamp"]
#endif

makeSymlinks :: FilePath -> [FilePath] -> IO ()
makeSymlinks src links = forM_ links $ \symlink -> do
  exists <- doesFileExist symlink
  when exists $ removeFile symlink
  createSymbolicLink src symlink
