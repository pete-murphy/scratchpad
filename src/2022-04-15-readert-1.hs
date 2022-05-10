{-# LANGUAGE FlexibleContexts #-}

import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Monad.Reader
import Say

data Env
  = Env
      { envLog :: !(String -> IO ()),
        envBalance :: !(TVar Int)
      }

modify ::
  (MonadReader Env m, MonadIO m) =>
  (Int -> Int) ->
  m ()
modify f = do
  balance <- asks envBalance
  liftIO (atomically (modifyTVar' balance f))

logSomething ::
  (MonadReader Env m, MonadIO m) =>
  String ->
  m ()
logSomething msg = do
  log <- asks envLog
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
