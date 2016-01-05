{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import Data.List
import Data.List.Split
import System.Directory
import System.Environment
import System.FilePath
import System.Process

main :: IO ()
main = getArgs >>= mapM_ redo

redo :: FilePath -> IO ()
redo f = do
  p <- outdated f
  when p $ runDo f

outdated :: FilePath -> IO Bool
outdated f = do
  x <- doesFileExist f
  if x
    then or <$> (getDeps f >>= mapM outdated')
    else return True

outdated' :: (FilePath, String) -> IO Bool
outdated' (f, oldMD5) = do
  x <- doesFileExist f
  if x
    then do newMD5 <- fileMD5 f
            if oldMD5 /= newMD5
              then return True
              else outdated f
    else return True

fileMD5 :: FilePath -> IO String
fileMD5 f = show . MD5.md5 <$> BL.readFile f

getDeps :: FilePath -> IO [(FilePath, String)]
getDeps f = handle (\(_ :: SomeException) -> return []) $ map read . lines <$> readFile (".redo" </> f)

runDo :: FilePath -> IO ()
runDo target = do
  (doFile, baseName) <- head <$> filterM (doesFileExist . fst) (doFiles target)
  callCommand $ cmds doFile baseName
  renameFile tmpOut target
  where cmds doFile baseName = unwords ["sh", "-ev", doFile, target, baseName, tmpOut, ">", tmpOut]
        tmpOut = target ++ "--redoing"

doFiles :: FilePath -> [(FilePath, FilePath)]
doFiles target = (target <.> "do", target) : defaultDos
  where tokens = splitOn "." target
        defaultDos = tail . map (toFileName *** intercalate ".") $ zip (tails tokens) (inits tokens)
        toFileName = intercalate "." . ("default":) . (++["do"])
