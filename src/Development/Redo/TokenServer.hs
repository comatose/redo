{-# LANGUAGE ScopedTypeVariables #-}

module Development.Redo.TokenServer (createProcessorTokens,
                                     destroyProcessorTokens,
                                     acquireProcessorToken,
                                     releaseProcessorToken,
                                     withProcessorToken,
                                     withoutProcessorToken
                                    )where

import qualified Development.Redo.Config as C

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Network.Socket
import System.IO.Unsafe

createProcessorTokens :: Int -> IO ()
createProcessorTokens n = when (C.parallelBuild > 1) $ do
  _ <- forkOS $ server 15907 n
  return ()

{-# NOINLINE clientSocket #-}
clientSocket :: Socket
clientSocket = unsafePerformIO $ socket AF_INET Datagram defaultProtocol

sendToServer :: String -> IO ()
sendToServer msg = do
  _ <- sendTo clientSocket msg (SockAddrInet 15907 localHost)
  return ()

recvFromServer :: String -> IO String
recvFromServer msg = do
  _ <- sendTo clientSocket msg (SockAddrInet 15907 localHost)
  (msg', _, _) <- recvFrom clientSocket 10
  return msg'

destroyProcessorTokens :: IO ()
destroyProcessorTokens = when (C.parallelBuild > 1) $ do
  sendToServer "shutdown"
  takeMVar mvar

acquireProcessorToken :: IO ()
acquireProcessorToken = when (C.parallelBuild > 1) $ do
  msg <- recvFromServer "wait"
  assert (msg == "ack") $ return ()

releaseProcessorToken :: IO ()
releaseProcessorToken = when (C.parallelBuild > 1) $ sendToServer "post"

withProcessorToken :: IO a -> IO a
withProcessorToken = bracket_ acquireProcessorToken releaseProcessorToken

withoutProcessorToken :: IO a -> IO a
withoutProcessorToken = bracket_ releaseProcessorToken acquireProcessorToken

{-# NOINLINE localHost #-}
localHost :: HostAddress
localHost = unsafePerformIO $ inet_addr "127.0.0.1"

{-# NOINLINE mvar #-}
mvar :: MVar ()
mvar = unsafePerformIO newEmptyMVar

server :: PortNumber -> Int -> IO ()
server port value = bracket enter exit $ \s -> do
  (_, (v, _)) <- runStateT (go s) (value, [])
  liftIO . C.printDebug $ "final tokens = " ++ show v
  return ()
 where
   enter = do
     s <- liftIO $ socket AF_INET Datagram defaultProtocol
     liftIO $ bind s (SockAddrInet port localHost)
     liftIO $ C.printDebug "token server start"
     return s
   exit s = do
     liftIO $ close s
     liftIO $ C.printDebug "token server stop"
     putMVar mvar ()

   go s = do
     (msg, _, client) <- liftIO $ recvFrom s 10 `onException` C.printDebug "error on recvFrom"
     case msg of
       "wait" -> do
         v <- countTokens
         if v == 0
           then enqueueRequest client
           else do _ <- liftIO $ sendTo s "ack" client
                   decreaseToken
         go s
       "post" -> do
         addr' <- dequeueRequest
         case addr' of
           Nothing -> increaseToken
           Just addr -> do
             increaseToken
             decreaseToken
             _ <- liftIO $ sendTo s "ack" addr
             return ()
         go s
       _ -> do
         liftIO $ C.printDebug "stop issued"
         (_, q) <- get
         mapM_ (liftIO . sendTo s "ack" ) q

countTokens :: StateT (Int, [SockAddr]) IO Int
countTokens = fst <$> get

decreaseToken :: StateT (Int, [SockAddr]) IO ()
decreaseToken = do
  (v, q) <- get
  put (v - 1, q)
  liftIO . C.printDebug $ "tokens = " ++ show (v - 1) ++ "(-)"

increaseToken :: StateT (Int, [SockAddr]) IO ()
increaseToken = do
  (v, q) <- get
  put (v + 1, q)
  liftIO . C.printDebug $ "tokens = " ++ show (v + 1) ++ "(+)"

enqueueRequest :: SockAddr -> StateT (Int, [SockAddr]) IO ()
enqueueRequest r = do
  (v, q) <- get
  put (v, q ++ [r])

dequeueRequest :: StateT (Int, [SockAddr]) IO (Maybe SockAddr)
dequeueRequest = do
  (v, q) <- get
  if null q
    then return Nothing
    else do put (v, tail q)
            return . Just $ head q
