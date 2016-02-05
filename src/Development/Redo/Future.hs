module Development.Redo.Future (Future,
                                asyncIO,
                                asyncOS,
                                cancel,
                                tryWait,
                                tryWaitEither,
                                wait,
                                waitEither
                               ) where

import Control.Concurrent
import Control.Exception

data Future a = Future ThreadId (MVar (Either SomeException a))

asyncIO :: IO a -> IO (Future a)
asyncIO io = do
  mvar <- newEmptyMVar
  tid <- forkIO $ handle (putMVar mvar . Left) (io >>= putMVar mvar . Right)
  return $ Future tid mvar

asyncOS :: IO a -> IO (Future a)
asyncOS io = do
  mvar <- newEmptyMVar
  tid <- forkOS $ handle (putMVar mvar . Left) (io >>= putMVar mvar . Right)
  return $ Future tid mvar

wait :: Future a -> IO a
wait (Future _ mvar) = readMVar mvar >>= either throwIO return

waitEither :: Future a -> IO (Either SomeException a)
waitEither (Future _ mvar) = readMVar mvar

tryWait :: Future a -> IO (Maybe a)
tryWait (Future _ mvar) = do
  r <- tryReadMVar mvar
  case r of
    Nothing -> return Nothing
    Just (Left e) -> throwIO e
    Just (Right a) -> return $ Just a

tryWaitEither :: Future a -> IO (Maybe (Either SomeException a))
tryWaitEither (Future _ mvar) = tryReadMVar mvar

cancel :: Future a -> IO ()
cancel (Future tid _) = killThread tid
