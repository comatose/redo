{-# LANGUAGE ScopedTypeVariables #-}

module Development.Redo.Util (createFile,
                              createTempFile,
                              decodePath,
                              encodePath,
                              finalize,
                              forkChild,
                              ignoreIOExceptionM,
                              ignoreIOExceptionM_,
                              initialize,
                              makeRelative',
                              moveFile,
                              normalise',
                              quote,
                              spanM,
                              splitOn,
                              waitForChildren,
                              createProcessorTokens,
                              destroyProcessorTokens,
                              acquireProcessorToken,
                              releaseProcessorToken,
                              withProcessorToken,
                              withoutProcessorToken
                             ) where

import Development.Redo.Config
import Development.Redo.TokenServer

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Numeric
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Unsafe

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn seps = foldr go [[]]
  where go x acc = if x `elem` seps
                   then [] : acc
                   else (x : head acc) : tail acc

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

ignoreIOExceptionM :: a -> IO a -> IO a
ignoreIOExceptionM r = handle (\(_ :: IOException) -> return r)

ignoreIOExceptionM_ :: IO () -> IO ()
ignoreIOExceptionM_ = ignoreIOExceptionM ()

children :: MVar [MVar ()]
{-# NOINLINE children #-}
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkFinally io (\_ -> putMVar mvar ())

initialize :: RedoSettings -> IO ()
initialize settings = when (callDepth == 0) $ do
  setEnv envSessionID sessionID
  setEnv envShellOptions $ shellOpts settings
  setEnv envDebugMode . show $ debug settings
  setEnv envParallelBuild . show $ inPar settings
  createGlobalLock
  createProcessorTokens (inPar settings - 1)

finalize :: IO ()
finalize =  when (callDepth == 0) $ do
  destroyProcessorTokens
  destroyGlobalLock
