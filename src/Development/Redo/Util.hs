{-# LANGUAGE ScopedTypeVariables #-}
module Development.Redo.Util where

import Development.Redo.Config

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Numeric
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Posix.Process
import System.Posix.Semaphore
import System.Posix.Types

pathWords :: FilePath -> [FilePath]
pathWords = splitOn [pathSeparator]

pathUnwords :: [FilePath] -> FilePath
pathUnwords = intercalate [pathSeparator]

normalise' :: FilePath -> FilePath
normalise' = normalise . pathUnwords . go . pathWords
  where go (_:"..":xs) = go xs
        go (x:xs) = x:go xs
        go [] = []

makeRelative' :: FilePath -> FilePath -> FilePath
makeRelative' baseDir f = dir </> takeFileName f
  where dir = pathUnwords $ go (pathWords baseDir) (pathWords $ takeDirectory f)
        go [] [] = []
        go [] ys = ys
        go xs [] = replicate (length xs) ".."
        go xall@(x:xs) yall@(y:ys)
          | x == y = go xs ys
          | otherwise = replicate (length xall) ".." ++ yall

encodePath :: FilePath -> FilePath
encodePath fp = encodePathS fp ""
  where encodePathS (x:xs)
          | x == pathSeparator = encodeChar x . encodePathS xs
          | x == '%'           = showString "%%" . encodePathS xs
          | otherwise          = showChar x . encodePathS xs
        encodePathS [] = const ""
        encodeChar c = showChar '%' . showHex (ord c)

decodePath :: FilePath -> FilePath
decodePath ('%':'%':xs) = '%' : decodePath xs
decodePath ('%':x:y:xs) = decoded : decodePath xs
 where decoded = chr . fst . head $ readHex (x:[y])
decodePath (x:xs) = x : decodePath xs
decodePath [] = []

quote :: String -> String
quote s = '\"' : s ++ "\""

createFile :: FilePath -> IO ()
createFile path = do
  createDirectoryIfMissing True (takeDirectory path)
  h <- openFile path WriteMode
  hClose h

createTempFile :: FilePath -> String -> IO FilePath
createTempFile path name = do
  createDirectoryIfMissing True path
  (f, h) <- openTempFile path name
  hClose h
  return f

moveFile :: FilePath -> FilePath -> IO ()
moveFile src trg = do
  exist <- doesFileExist src
  when exist $ do
    createDirectoryIfMissing True $ takeDirectory trg
    renameFile src trg

spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _ [] = return ([], [])
spanM p xall@(x:xs) = do
  p' <- p x
  if p'
    then do
      (ys, zs) <- spanM p xs
      return (x:ys, zs)
    else return ([], xall)

ignoreExceptionM :: a -> IO a -> IO a
ignoreExceptionM r = handle (\(_ :: SomeException) -> return r)

ignoreExceptionM_ :: IO () -> IO ()
ignoreExceptionM_ = ignoreExceptionM ()

createProcessorTokens :: Int -> IO ()
createProcessorTokens n = do
  pid <- getProcessID
  let sid = semaphorePrefix ++ show pid
  _ <- semOpen sid (OpenSemFlags True True) (CMode 448) n
  printDebug $ "Semaphore '" ++ sid ++ "' with " ++ show n ++ " sems created."
  setEnv envSemaphoreID sid

destroyProcessorTokens :: IO ()
destroyProcessorTokens = do
  sid <- getEnv envSemaphoreID
  sem <- semOpen sid (OpenSemFlags False False) (CMode 448) 0
  n <- semGetValue sem
  printDebug $ "Semaphore '" ++ sid ++ "' with " ++ show n ++ " sems destroyed."
  semUnlink sid

getSemaphoreID :: IO Semaphore
getSemaphoreID = do
  sid <- getEnv envSemaphoreID
  semOpen sid (OpenSemFlags True False) (CMode 448) 0

withProcessorToken :: Semaphore -> IO a -> IO a
withProcessorToken sem = bracket_ (semThreadWait sem >> printProcessorTokens sem)
  (semPost sem >> printProcessorTokens sem)

withoutProcessorToken :: Semaphore -> IO a -> IO a
withoutProcessorToken sem = bracket_ (semPost sem >> printProcessorTokens sem)
  (semThreadWait sem >> printProcessorTokens sem)

printProcessorTokens :: Semaphore -> IO ()
printProcessorTokens sem = do
  n <- semGetValue sem
  printDebug $ "Semaphores = " ++ show n
