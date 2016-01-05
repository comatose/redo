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
import System.IO
import System.Process

configPath :: FilePath
configPath = ".redo"

tempPath :: FilePath
tempPath = configPath </> "tmp"

main :: IO ()
main = do
  cmd <- getProgName
  case cmd of
    "redo2" -> getArgs >>= mapM_ redo
    "redo-ifchange" -> do
      maybeDepsPath <- getDepsPath
      case maybeDepsPath of
        (Just depsPath) -> do
          deps <- getArgs
          md5s <- mapM redo deps
          zipWithM_ (addDeps depsPath) deps md5s
        Nothing -> return ()
    _ -> return ()

getDepsPath :: IO (Maybe String)
getDepsPath = lookupEnv "REDO_DEPS_PATH"

redo :: FilePath -> IO String
redo f = do
  p <- upToDate f
  if p
    then hPutStrLn stderr (f ++ " is up to date.")
    else do
      hPutStrLn stderr $ "redo " ++ f
      runDo f
  fileMD5 f

addDeps :: FilePath -> FilePath -> String -> IO ()
addDeps target dep md5 = withFile target AppendMode (`hPutStrLn` show (dep, md5))

upToDate :: FilePath -> IO Bool
upToDate f = do
  x <- doesFileExist f
  if x
    then do
      maybeDeps <- getDeps f
      case maybeDeps of
        (Just deps) -> if null deps
                       then return True
                       else and <$> mapM upToDate' deps
        Nothing -> return False
    else return False

upToDate' :: (FilePath, String) -> IO Bool
upToDate' (f, oldMD5) = do
  x <- doesFileExist f
  if x
    then do newMD5 <- fileMD5 f
            if oldMD5 /= newMD5
              then return False
              else upToDate f
    else hPutStrLn stderr (f ++ " not exist.") >> return False

fileMD5 :: FilePath -> IO String
fileMD5 f = show . MD5.md5 <$> BL.readFile f

-- TODO: add do files into dependencies
getDeps :: FilePath -> IO (Maybe [(FilePath, String)])
getDeps f = handle
  (\(_ :: SomeException) -> return Nothing)
  (Just . map read . lines <$> readFile (configPath </> f))

runDo :: FilePath -> IO ()
runDo target = do
  (baseName, doFile) <- head <$> filterM (doesFileExist . snd) (doFiles target)
  tmpDeps <- createTempFile tempPath target
  tmpOut <- createTempFile tempPath target
  callCommand $ cmds tmpDeps doFile baseName tmpOut
  renameFile tmpOut target
  renameFile tmpDeps (configPath </> target)
  where cmds tmpDeps doFile baseName tmpOut
          = unwords ["REDO_DEPS_PATH='" ++ tmpDeps ++ "'",
                     "sh -ev", doFile, target, baseName,
                     tmpOut, ">", tmpOut]

createTempFile :: FilePath -> String -> IO FilePath
createTempFile path baseName = do
  createDirectoryIfMissing True path
  (f, h) <- openTempFile path baseName
  hClose h
  return f

doFiles :: FilePath -> [(FilePath, FilePath)]
doFiles target = (target, target <.> "do") : defaultDos
  where tokens = splitOn "." target
        defaultDos = map ((intercalate "." *** toFileName) . (`splitAt` tokens)) [1..length tokens]
        toFileName = intercalate "." . ("default":) . (++["do"])
