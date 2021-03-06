{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
--
-- This is basically a semaphore. I made this because 'sem_wait(3)'
-- terminates the application without throwing an exception and there
-- is no chance to clean up resources. There is a version also
-- implemented using real semaphores included in
-- Development.Redo.TokenSem, which works well for redo using delegate
-- processes not threads.
--
-----------------------------------------------------------------------------

module Development.Redo.TokenServer (createProcessorTokens,
                                     destroyProcessorTokens,
                                     acquireProcessorToken,
                                     releaseProcessorToken,
                                     withProcessorToken,
                                     withoutProcessorToken
                                    ) where

import qualified Development.Redo.Config as C

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.PQueue.Prio.Max as Q
import Network.Socket
import System.Directory
import System.FilePath.Posix
import System.IO.Unsafe
import System.Posix.Process
import System.Posix.Types

createProcessorTokens :: Int -> IO ()
createProcessorTokens n = when (C.parallelBuild > 1) $ do
  _ <- forkOS $ server n
  return ()

{-# NOINLINE clientSocket #-}
clientSocket :: Socket
clientSocket = unsafePerformIO $ do
  s <- socket AF_UNIX Datagram defaultProtocol
  bind s clientAddr
  return s

sendToServer :: String -> IO ()
sendToServer msg = do
  _ <- sendTo clientSocket msg serverAddr
  return ()

recvFromServer :: String -> IO String
recvFromServer msg = do
  _ <- sendTo clientSocket msg serverAddr
  (msg', _, _) <- recvFrom clientSocket 10
  return msg'

destroyProcessorTokens :: IO ()
destroyProcessorTokens = when (C.parallelBuild > 1) $ do
  sendToServer "shutdown"
  takeMVar mvar
  removeDirectoryRecursive socketDirPath `catch` (\(_::IOException) -> return ())

acquireProcessorToken :: IO ()
acquireProcessorToken = when (C.parallelBuild > 1) $ do
  msg <- recvFromServer ('w':show C.callDepth)  -- send wait using call depth as a priority.
  assert (msg == "ack") $ return ()

releaseProcessorToken :: IO ()
releaseProcessorToken = when (C.parallelBuild > 1) $ sendToServer "post"

withProcessorToken :: IO a -> IO a
withProcessorToken = bracket_ acquireProcessorToken releaseProcessorToken

withoutProcessorToken :: IO a -> IO a
withoutProcessorToken = bracket_ releaseProcessorToken acquireProcessorToken

{-# NOINLINE processID #-}
processID :: ProcessID
processID = unsafePerformIO getProcessID

-- | This is the directory where temporary files are created.
{-# NOINLINE socketDirPath #-}
socketDirPath :: FilePath
socketDirPath = unsafePerformIO $ do
  let p = C.tempDirPath </> "uds"
  createDirectoryIfMissing True p
  return p

serverName :: String
serverName = socketDirPath </> "token_" ++ C.sessionID

clientName :: String
clientName = serverName ++ "_" ++ show processID

serverAddr :: SockAddr
serverAddr = SockAddrUnix serverName

clientAddr :: SockAddr
clientAddr = SockAddrUnix clientName

{-# NOINLINE mvar #-}
mvar :: MVar ()
mvar = unsafePerformIO newEmptyMVar

server :: Int -> IO ()
server value = bracket enter exit $ \s -> do
  (_, (v, _)) <- runStateT (go s) (value, Q.empty)
  liftIO . C.printDebug $ "final tokens = " ++ show v
  return ()
 where
   enter = do
     s <- liftIO $ socket AF_UNIX Datagram defaultProtocol
     liftIO $ bind s serverAddr
     liftIO $ C.printDebug "token server start"
     return s
   exit s = do
     liftIO $ close s
     liftIO $ C.printDebug "token server stop"
     putMVar mvar ()

   go s = do
     (msg, _, client) <- liftIO $ recvFrom s 128 `onException` C.printDebug "error on recvFrom"
     case msg of
       ('w':priority) -> do
         v <- countTokens
         if v == 0
           then enqueueRequest (read priority, client)
           else do _ <- liftIO $ ack s client
                   decreaseToken
         go s
       "post" -> do
         addr' <- dequeueRequest
         case addr' of
           Nothing -> increaseToken
           Just addr -> do
             when C.debugMode $ do
               increaseToken
               decreaseToken
             _ <- liftIO $ ack s addr
             return ()
         go s
       _ -> get >>= mapM_ (ack s) . Q.elems . snd

   ack s client = liftIO $ sendTo s "ack" client `catch` \(_::IOException) -> return (-1)

type ClientQueue = Q.MaxPQueue Int SockAddr

countTokens :: StateT (Int, ClientQueue) IO Int
countTokens = fst <$> get

decreaseToken :: StateT (Int, ClientQueue) IO ()
decreaseToken = do
  (v, q) <- get
  put (v - 1, q)
  liftIO . C.printDebug $ "tokens = " ++ show (v - 1) ++ "(-)"

increaseToken :: StateT (Int, ClientQueue) IO ()
increaseToken = do
  (v, q) <- get
  put (v + 1, q)
  liftIO . C.printDebug $ "tokens = " ++ show (v + 1) ++ "(+)"

enqueueRequest :: (Int, SockAddr) -> StateT (Int, ClientQueue) IO ()
enqueueRequest (p, r) = do
  (v, q) <- get
  put (v, Q.insert p r q)

dequeueRequest :: StateT (Int, ClientQueue) IO (Maybe SockAddr)
dequeueRequest = do
  (v, q) <- get
  if Q.null q
    then return Nothing
    else do let (_, r) = Q.findMax q
            put (v, Q.deleteMax q)
            return $ Just r
