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

getDeps :: FilePath -> IO (Maybe [(FilePath, Signature)])
getDeps f = handle
  (\(_ :: SomeException) -> return Nothing)
  (Just . map read . lines <$> readFile (configPath </> f))

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

redo :: FilePath -> IO Signature
redo f = do
  p <- upToDate f AnySignature
  if p
    then hPutStrLn stderr (f ++ " is up to date.")
    else do
      hPutStrLn stderr $ "redo " ++ f
      r <- runDo f
      unless r $ error "Failed."
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

runDo :: FilePath -> IO Bool
runDo target = do
  doFiles <- filterM (doesFileExist . snd) (listDoFiles target)
  if null doFiles
    then handleNoDo target
    else executeDo target $ head doFiles

handleNoDo :: FilePath -> IO Bool
handleNoDo target = do
  x <- doesFileExist target
  if x
    then do createFile $ configPath </> target
            return True
    else return False

executeDo :: FilePath -> (String, FilePath) -> IO Bool
executeDo target (baseName, doFile) = do
  createFile $ configPath </> doFile
  tmpDeps <- createTempFile tempPath target
  tmpOut <- createTempFile tempPath target
  fileSignature doFile >>= addDeps tmpDeps doFile
  hPutStrLn stderr $ "runDo " ++ doFile
  callCommand $ cmds tmpDeps tmpOut
  renameFile tmpOut target
  renameFile tmpDeps (configPath </> target)
  return True
    where cmds tmpDeps tmpOut
            = unwords ["REDO_DEPS_PATH='" ++ tmpDeps ++ "'",
                       "sh -ex", doFile, target, baseName, tmpOut]

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
