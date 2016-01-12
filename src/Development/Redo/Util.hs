{-# LANGUAGE ScopedTypeVariables #-}
module Development.Redo.Util where

import Control.Exception
import Data.List
import Data.List.Split
import System.Directory
import System.FilePath
import System.IO

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
encodePath ('.':xs) = "_." ++ encodePath xs
encodePath (x:xs) = x : encodePath xs
encodePath [] = []

decodePath :: FilePath -> FilePath
decodePath ('_':'.':xs) = '.' : decodePath xs
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
