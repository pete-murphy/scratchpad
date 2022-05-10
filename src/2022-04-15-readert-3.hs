{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Control.Monad.State.Strict as State
import Say

data Env
  = Env
      { envLog :: !(String -> IO ()),
        envBalance :: !(TVar Int)
      }

class HasLog a where
  getLog :: a -> (String -> IO ())

instance HasLog (String -> IO ()) where
  getLog = id

instance HasLog Env where
  getLog = envLog

class HasBalance a where
  getBalance :: a -> TVar Int

instance HasBalance (TVar Int) where
  getBalance = id

instance HasBalance Env where
  getBalance = envBalance

class Monad m => MonadBalance m where
  modifyBalance :: (Int -> Int) -> m ()

instance (HasBalance env, MonadIO m) => MonadBalance (ReaderT env m) where
  modifyBalance f = do
    balance <- asks getBalance
    liftIO (atomically (modifyTVar' balance f))

instance Monad m => MonadBalance (State.StateT Int m) where
  modifyBalance = State.modify

-- modify ::
--   (MonadReader env m, HasBalance env, MonadIO m) =>
--   (Int -> Int) ->
--   m ()
-- modify f = do
--   balance <- asks getBalance
--   liftIO (atomically (modifyTVar' balance f))
modify :: MonadBalance m => (Int -> Int) -> m ()
modify = modifyBalance

logSomething ::
  (MonadReader env m, HasLog env, MonadIO m) =>
  String ->
  m ()
logSomething msg = do
  log <- asks getLog
  liftIO (log msg)

main :: IO ()
main = do
  ref <- newTVarIO 4
  let env =
        Env
          { envLog = sayString,
            envBalance = ref
          }
  runReaderT
    ( concurrently
        (modify (+ 1))
        (logSomething "Increasing account balance")
    )
    env
  balance <- readTVarIO ref
  sayString ("Final balance: " <> show balance)
