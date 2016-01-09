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

addDependency :: FilePath -> RedoTarget -> Signature -> IO ()
addDependency depsFile dep sig =
  withFile depsFile AppendMode (`hPutStrLn` show (targetPath dep, sig))

newtype RedoTarget = RedoTarget {targetPath :: String} deriving (Show, Read)

redoTarget :: FilePath -> IO RedoTarget
redoTarget target = flip redoTargetFromDir target <$> getCurrentDirectory

redoTargetFromDir :: FilePath -- ^ base directory, this should be an absolute path.
                  -> FilePath
                  -> RedoTarget
redoTargetFromDir baseDir target =
  if isRelative $ takeDirectory target'
    then RedoTarget target'
    else RedoTarget $ makeRelative' baseDir target'
  where target' = normalise' target

main :: IO ()
main = do
  dir <- getCurrentDirectory
  cmd <- getProgName
  case cmd of
    "redo" -> getArgs >>= mapM_ (redo . redoTargetFromDir dir)
    "redo-ifchange" -> do
      maybeDepsPath <- getDepsPath
      case maybeDepsPath of
        (Just depsPath) -> do
          deps <- map (redoTargetFromDir dir) <$> getArgs
          sigs <- mapM redo deps
          zipWithM_ (addDependency depsPath) deps sigs
        Nothing -> return ()
    _ -> hPrint stderr $ "unknown command: " ++ cmd

getDepsPath :: IO (Maybe String)
getDepsPath = lookupEnv "REDO_DEPS_PATH"

getCallDepth :: IO Int
getCallDepth = handle
  (\(_ :: SomeException) -> return 0)
  (read <$> getEnv "REDO_CALL_DEPTH")

redo :: RedoTarget -> IO Signature
redo target = do
  callDepth <- getCallDepth
  replicateM_ callDepth $ hPutChar stderr ' '
  hPutStrLn stderr $ "redo  " ++ targetPath target
  p <- upToDate target AnySignature
  if p
    then hPutStrLn stderr $ targetPath target ++ " is up to date."
    else runDo target
         `catch`
         \e -> case e of
           (NoDoFileExist t) -> hPutStrLn stderr $ "no rule to make " ++ quote t
           (DoExitFailure err) -> hPutStrLn stderr $ targetPath target ++ " failed with exitcode " ++ show err
  fileSignature $ targetPath target

upToDate :: RedoTarget -> Signature -> IO Bool
upToDate target oldSig = do
  newSig <- fileSignature . targetPath $ target
  if oldSig /= newSig
    then return False
    else do
      maybeDeps <- getDependencies target
      -- print (target, maybeDeps)
      case maybeDeps of
        (Just deps) -> if null deps
                       then return True
                       else and <$> mapM (uncurry upToDate) deps
        Nothing -> return False

depFilePath :: RedoTarget -> FilePath
depFilePath target = configPath </> encodePath (targetPath target)

getDependencies :: RedoTarget -> IO (Maybe [(RedoTarget, Signature)])
getDependencies target = handle
  (\(_ :: SomeException) -> return Nothing)
  (do dir <- getCurrentDirectory
      depLines <- lines <$> readFile (depFilePath target)
      return . Just . map (first (redoTargetFromDir dir) . read) $ depLines)

runDo :: RedoTarget -> IO ()
runDo target = do
  doFiles <- filterM (doesFileExist . snd) (listDoFiles target)
  if null doFiles
    then handleNoDo target
    else executeDo target $ head doFiles

handleNoDo :: RedoTarget -> IO ()
handleNoDo target = do
  callDepth <- getCallDepth
  exists <- doesFileExist $ targetPath target
  if callDepth == 0 || not exists
    then throwIO . NoDoFileExist $ targetPath target
    else createFile $ depFilePath target

executeDo :: RedoTarget -> (String, FilePath) -> IO ()
executeDo target (baseName, doFile) = do
  doFileTarget <- redoTarget doFile
  createFile $ depFilePath doFileTarget
  tmpDeps <- createTempFile tempPath . takeFileName $ targetPath target
  tmpOut <- createTempFile tempPath . takeFileName $ targetPath target
  fileSignature doFile >>= addDependency tmpDeps doFileTarget
  callDepth <- getCallDepth
  print $ cmds tmpDeps (callDepth + 1) tmpOut
  ph <- spawnCommand $ cmds tmpDeps (callDepth + 1) tmpOut
  ec <- waitForProcess ph
  case ec of
    ExitSuccess -> do
      renameFile tmpOut $ targetPath target
      renameFile tmpDeps $ depFilePath target
    ExitFailure err -> do
      removeFile tmpOut
      removeFile tmpDeps
      throwIO $ DoExitFailure err

  where cmds tmpDeps callDepth tmpOut
          = unwords ["REDO_DEPS_PATH=" ++ quote tmpDeps,
                     "REDO_CALL_DEPTH=" ++ show callDepth,
                     "sh -e", quote doFile, quote $ targetPath target,
                     quote baseName, quote tmpOut]

listDoFiles :: RedoTarget -> [(String, FilePath)]
listDoFiles (RedoTarget target) = (target, takeFileName target <.> "do") : defaultDos
  where tokens = splitOn "." (takeFileName target)
        defaultDos = map ((toBaseName *** toFileName) . (`splitAt` tokens)) [1..length tokens]
        toBaseName xs = normalise' $ takeDirectory target </> intercalate "." xs
        toFileName = intercalate "." . ("default":) . (++["do"])
