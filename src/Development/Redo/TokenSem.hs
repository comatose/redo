{-# LANGUAGE ScopedTypeVariables #-}

module Development.Redo.TokenSem (acquireProcessorToken,
                                  createProcessorTokens,
                                  destroyProcessorTokens,
                                  releaseProcessorToken,
                                  withoutProcessorToken,
                                  withProcessorToken,
                                 ) where

import qualified Development.Redo.Config as C

import Control.Exception
import Control.Monad
import System.IO.Unsafe
import System.Posix.Semaphore
import System.Posix.Types

createProcessorTokens :: Int -> IO ()
createProcessorTokens n = when (C.parallelBuild > 1) $ do
  _ <- semOpen semaphoreID (OpenSemFlags True True) (CMode 448) n
  C.printDebug $ "Semaphore '" ++ semaphoreID ++ "' with " ++ show n ++ " sems created."

destroyProcessorTokens :: IO ()
destroyProcessorTokens = when (C.parallelBuild > 1) $ do
  n <- semGetValue semaphore
  C.printDebug $ "Semaphore '" ++ semaphoreID ++ "' with " ++ show n ++ " sems destroyed."
  semUnlink semaphoreID

semaphoreID :: String
semaphoreID = "/redo_sem_" ++ C.sessionID

semaphore :: Semaphore
{-# NOINLINE semaphore #-}
semaphore = unsafePerformIO $ semOpen semaphoreID (OpenSemFlags False False) (CMode 448) 0

acquireProcessorToken :: IO ()
acquireProcessorToken = when (C.parallelBuild > 1) $
  bracketOnError (semWait semaphore) (const $ semPost semaphore)
  (const $ printProcessorTokens semaphore)

releaseProcessorToken :: IO ()
releaseProcessorToken = when (C.parallelBuild > 1) $
  bracketOnError (semPost semaphore) (const $ semWait semaphore)
  (const $ printProcessorTokens semaphore)

withProcessorToken :: IO a -> IO a
withProcessorToken action = if C.parallelBuild > 1
                            then bracket_ (semWait semaphore) (semPost semaphore) action
                            else action

withoutProcessorToken :: IO a -> IO a
withoutProcessorToken action = if C.parallelBuild > 1
                               then bracket_ (semPost semaphore) (semWait semaphore) action
                               else action

printProcessorTokens :: Semaphore -> IO ()
printProcessorTokens sem = do
  n <- semGetValue sem
  C.printDebug $ "Semaphores = " ++ show n
