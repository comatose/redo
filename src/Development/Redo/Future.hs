module Development.Redo.Future (Future,
                                async,
                                wait
                               ) where

import Control.Concurrent
import Control.Exception

data Future a = Future (MVar (Either SomeException a))

async :: IO a -> IO (Future a)
async io = do
  mvar <- newEmptyMVar
  _ <- forkIO $ handle (putMVar mvar . Left) (io >>= putMVar mvar . Right)
  return $ Future mvar

-- wait :: Future a -> IO ()
-- wait (Future mvar) = do
--   _ <- readMVar mvar
--   return ()

wait :: Future a -> IO (Either SomeException a)
wait (Future mvar) = readMVar mvar
