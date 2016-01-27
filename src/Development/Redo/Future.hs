module Development.Redo.Future (Future,
                                async,
                                cancel,
                                wait
                               ) where

import Control.Concurrent
import Control.Exception

data Future a = Future ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Future a)
async io = do
  mvar <- newEmptyMVar
  tid <- forkIO $ handle (putMVar mvar . Left) (io >>= putMVar mvar . Right)
  return $ Future tid mvar

wait :: Future a -> IO a
wait (Future _ mvar) = readMVar mvar >>= either throwIO return

cancel :: Future a -> IO ()
cancel (Future tid _) = killThread tid
