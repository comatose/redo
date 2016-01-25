module Development.Redo.Util (Future,
                              async,
                              waitAndGet
                             ) where

import Control.Concurrent
import Control.Exception

data Future a = Future (MVar (Either SomeException a))

async :: IO a -> IO (Future a)
async io = do
  mvar <- newEmptyMVar
  _ <- forkIO $ handle (putMVar mvar . Left) (io >>= putMVar mvar . Right)
  return $ Future mvar

waitAndGet :: Future a -> IO (Either SomeException a)
waitAndGet (Future mvar) = takeMVar mvar
