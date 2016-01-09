{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import Data.List
import Data.List.Split
import Data.Typeable
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

configPath :: FilePath
configPath = ".redo"

tempPath :: FilePath
tempPath = configPath </> "tmp"

data RedoException =
  NoDoFileExist FilePath |
  DoExitFailure Int
  deriving (Show, Typeable)

instance Exception RedoException

data Signature = NoSignature | AnySignature | Signature String deriving (Show, Read)

instance Eq Signature where
  (Signature x) == (Signature y) = x == y
  NoSignature == _ = False
  _ == NoSignature = False
  _ == _ = True

fileSignature :: FilePath -> IO Signature
fileSignature f = handle
  (\(_ :: SomeException) -> return NoSignature)
  (Signature . show . MD5.md5 <$> BL.readFile f)

addDeps :: FilePath -> FilePath -> Signature -> IO ()
addDeps target dep sig = withFile target AppendMode (`hPutStrLn` show (dep, sig))

data RedoTarget = RedoTarget {absoluteDir :: String, relativeDir :: String, baseName :: String} deriving (Show, Read)

redoTarget target = do
  baseDir <- getCurrentDirectory
  if isRelative directory
    then return $ RedoTarget (baseDir </> directory) directory targetName
    else return $ RedoTarget directory (makeRelative baseDir directory) targetName
  where targetName = takeBaseName target
        directory = takeDirectory target

(<//>) :: FilePath -> FilePath -> FilePath
x <//> y = normalise $ x ++ (pathSeparator : y)

normaliseEx :: FilePath -> FilePath
normaliseEx = normalise . intercalate [pathSeparator] . go . splitOn [pathSeparator]
  where go (_:"..":xs) = go xs
        go (x:xs) = x:go xs
        go [] = []

makeRelativeEx :: FilePath -> FilePath -> FilePath
makeRelativeEx baseDir f = intercalate [pathSeparator] $ go (splitOn [pathSeparator] baseDir) (splitOn [pathSeparator] $ takeDirectory f)

go :: [FilePath] -> [FilePath] -> [FilePath]
go [] [] = []
go [] ys = ys
go xs [] = replicate (length xs) ".."
go xall@(x:xs) yall@(y:ys)
  | x == y = go xs ys
  | otherwise = replicate (length xall) ".." ++ yall

absolutePath :: RedoTarget -> FilePath
absolutePath target = absoluteDir target </> baseName target

relativePath :: RedoTarget -> FilePath
relativePath target = relativeDir target </> baseName target

main :: IO ()
main = do
  cmd <- getProgName
  case cmd of
    "redo" -> getArgs >>= mapM_ redo
    "redo-ifchange" -> do
      maybeDepsPath <- getDepsPath
      case maybeDepsPath of
        (Just depsPath) -> do
          deps <- getArgs
          sigs <- mapM redo deps
          zipWithM_ (addDeps depsPath) deps sigs
        Nothing -> return ()
    _ -> hPrint stderr $ "unknown command: " ++ cmd

getDepsPath :: IO (Maybe String)
getDepsPath = lookupEnv "REDO_DEPS_PATH"

getCallDepth :: IO Int
getCallDepth = handle
  (\(_ :: SomeException) -> return 0)
  (read <$> getEnv "REDO_CALL_DEPTH")

quote :: String -> String
quote s = '\'' : s ++ "'"

redo :: FilePath -> IO Signature
redo f = do
  callDepth <- getCallDepth
  replicateM_ callDepth $ hPutChar stderr ' '
  hPutStrLn stderr $ "redo  " ++ f
  p <- upToDate f AnySignature
  if p
    then hPutStrLn stderr $ f ++ " is up to date."
    else runDo f
         `catch`
         \e -> case e of
           (NoDoFileExist target) -> hPutStrLn stderr $ "no rule to make " ++ quote target
           (DoExitFailure err) -> hPutStrLn stderr $ f ++ " failed with exitcode " ++ show err
  fileSignature f

upToDate :: FilePath -> Signature -> IO Bool
upToDate file oldSig = do
  newSig <- fileSignature file
  if oldSig /= newSig
    then return False
    else do
      maybeDeps <- getDeps file
      case maybeDeps of
        (Just deps) -> if null deps
                       then return True
                       else and <$> mapM (uncurry upToDate) deps
        Nothing -> return False

getDeps :: FilePath -> IO (Maybe [(FilePath, Signature)])
getDeps f = handle
  (\(_ :: SomeException) -> return Nothing)
  (do f' <- canonicalizePath f
      s <- readFile (configPath <//> f')
      return . Just . map read . lines $ s)
  -- (Just . map read . lines <$> readFile (configPath </> f))

runDo :: FilePath -> IO ()
runDo target = do
  doFiles <- filterM (doesFileExist . snd) (listDoFiles target)
  if null doFiles
    then handleNoDo target
    else executeDo target $ head doFiles

handleNoDo :: FilePath -> IO ()
handleNoDo target = do
  callDepth <- getCallDepth
  if callDepth == 0
    then throwIO $ NoDoFileExist target
    else do
      x <- doesFileExist target
      if x
        then createFile $ configPath </> target
        else throwIO $ NoDoFileExist target

executeDo :: FilePath -> (String, FilePath) -> IO ()
executeDo target (baseName, doFile) = do
  createFile $ configPath </> doFile
  tmpDeps <- createTempFile tempPath (takeBaseName target)
  tmpOut <- createTempFile tempPath (takeBaseName target)
  fileSignature doFile >>= addDeps tmpDeps doFile
  callDepth <- getCallDepth
  print $ cmds tmpDeps (callDepth + 1) tmpOut
  ph <- spawnCommand $ cmds tmpDeps (callDepth + 1) tmpOut
  ec <- waitForProcess ph
  case ec of
    ExitSuccess -> do
      renameFile tmpOut target
      renameFile tmpDeps (configPath </> target)
    ExitFailure err -> do
      removeFile tmpOut
      removeFile tmpDeps
      throwIO $ DoExitFailure err

  where cmds tmpDeps callDepth tmpOut
          = unwords ["REDO_DEPS_PATH=" ++ quote tmpDeps,
                     "REDO_CALL_DEPTH=" ++ show callDepth,
                     "sh -e", doFile, target, baseName, tmpOut]

createFile :: FilePath -> IO ()
createFile path = do
  createDirectoryIfMissing True (takeDirectory path)
  h <- openFile path WriteMode
  hClose h

createTempFile :: FilePath -> String -> IO FilePath
createTempFile path baseName = do
  createDirectoryIfMissing True path
  (f, h) <- openTempFile path baseName
  hClose h
  return f

listDoFiles :: FilePath -> [(String, FilePath)]
listDoFiles target = (target, target <.> "do") : defaultDos
  where tokens = splitOn "." target
        defaultDos = map ((intercalate "." *** toFileName) . (`splitAt` tokens)) [1..length tokens]
        toFileName = intercalate "." . ("default":) . (++["do"])
