{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Development.Redo.Util

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

data RedoTarget = RedoTarget {absoluteDir :: String,
                              relativeDir :: String,
                              fileName :: String} deriving (Show, Read)

redoTarget :: FilePath -> IO RedoTarget
redoTarget target = (flip redoTargetFromDir) target <$> getCurrentDirectory

redoTargetFromDir :: FilePath -> FilePath -> RedoTarget
redoTargetFromDir baseDir target =
  if isRelative directory
    then RedoTarget (baseDir </> directory) directory targetName
    else RedoTarget directory (makeRelative' baseDir directory) targetName
  where targetName = takeFileName target
        directory = takeDirectory target

absolutePath :: RedoTarget -> FilePath
absolutePath target = absoluteDir target </> fileName target

relativePath :: RedoTarget -> FilePath
relativePath target = relativeDir target </> fileName target

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

redo :: FilePath -> IO Signature
redo f = do
  callDepth <- getCallDepth
  replicateM_ callDepth $ hPutChar stderr ' '
  target <- redoTarget f
  hPutStrLn stderr $ "redo  " ++ relativePath target
  p <- upToDate target AnySignature
  if p
    then hPutStrLn stderr $ f ++ " is up to date."
    else runDo target
         `catch`
         \e -> case e of
           (NoDoFileExist t) -> hPutStrLn stderr $ "no rule to make " ++ quote t
           (DoExitFailure err) -> hPutStrLn stderr $ f ++ " failed with exitcode " ++ show err
  fileSignature f

upToDate :: RedoTarget -> Signature -> IO Bool
upToDate target oldSig = do
  newSig <- fileSignature . relativePath $ target
  if oldSig /= newSig
    then return False
    else do
      maybeDeps <- getDeps target
      case maybeDeps of
        (Just deps) -> if null deps
                       then return True
                       else and <$> mapM (uncurry upToDate) deps
        Nothing -> return False

getDeps :: RedoTarget -> IO (Maybe [(RedoTarget, Signature)])
getDeps target = handle
  (\(_ :: SomeException) -> return Nothing)
  (do dir <- getCurrentDirectory
      depLines <- lines <$> readFile (configPath <//> absolutePath target)
      return . Just . map ((\(f, s) -> (redoTargetFromDir dir f, s)) . read) $ depLines)

runDo :: RedoTarget -> IO ()
runDo target = do
  doFiles <- filterM (doesFileExist . snd) (listDoFiles target)
  if null doFiles
    then handleNoDo target
    else executeDo target $ head doFiles

handleNoDo :: RedoTarget -> IO ()
handleNoDo target = do
  callDepth <- getCallDepth
  exists <- doesFileExist $ relativePath target
  if (callDepth == 0) || (not exists)
    then throwIO . NoDoFileExist $ relativePath target
    else createFile $ configPath </> fileName target

executeDo :: RedoTarget -> (String, FilePath) -> IO ()
executeDo target (baseName, doFile) = do
  createFile $ configPath </> doFile
  tmpDeps <- createTempFile tempPath (fileName target)
  tmpOut <- createTempFile tempPath (fileName target)
  fileSignature doFile >>= addDeps tmpDeps doFile
  callDepth <- getCallDepth
  print $ cmds tmpDeps (callDepth + 1) tmpOut
  ph <- spawnCommand $ cmds tmpDeps (callDepth + 1) tmpOut
  ec <- waitForProcess ph
  case ec of
    ExitSuccess -> do
      renameFile tmpOut $ relativePath target
      renameFile tmpDeps $ (configPath <//> absolutePath target)
    ExitFailure err -> do
      removeFile tmpOut
      removeFile tmpDeps
      throwIO $ DoExitFailure err

  where cmds tmpDeps callDepth tmpOut
          = unwords ["REDO_DEPS_PATH=" ++ quote tmpDeps,
                     "REDO_CALL_DEPTH=" ++ show callDepth,
                     "sh -e", doFile, fileName target, baseName, tmpOut]

listDoFiles :: RedoTarget -> [(String, FilePath)]
listDoFiles target = (relativePath target, fileName target <.> "do") : defaultDos
  where tokens = splitOn "." (fileName target)
        defaultDos = map ((toBaseName *** toFileName) . (`splitAt` tokens)) [1..length tokens]
        toBaseName xs = relativeDir target </> intercalate "." xs
        toFileName = intercalate "." . ("default":) . (++["do"])
