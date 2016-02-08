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
serverName = socketDirPath </> "token_server_" ++ C.sessionID

clientName :: String
clientName = socketDirPath </> "token_client_" ++ show processID

serverAddr :: SockAddr
serverAddr = SockAddrUnix serverName

clientAddr :: SockAddr
clientAddr = SockAddrUnix clientName

{-# NOINLINE mvar #-}
mvar :: MVar ()
mvar = unsafePerformIO newEmptyMVar

server :: Int -> IO ()
server value = bracket enter exit $ \s -> do
  (_, (v, _)) <- runStateT (go s) (value, [])
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
     (msg, _, client) <- liftIO $ recvFrom s 10 `onException` C.printDebug "error on recvFrom"
     case msg of
       "wait" -> do
         v <- countTokens
         if v == 0
           then enqueueRequest client
           else do _ <- liftIO $ ack s client
                   decreaseToken
         go s
       "post" -> do
         addr' <- dequeueRequest
         case addr' of
           Nothing -> increaseToken
           Just addr -> do
             increaseToken
             decreaseToken
             _ <- liftIO $ ack s addr
             return ()
         go s
       _ -> get >>= mapM_ (ack s) . snd

   ack s client = liftIO $ sendTo s "ack" client `catch` \(_::IOException) -> return (-1)

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
