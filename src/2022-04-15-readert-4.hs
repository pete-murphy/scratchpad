{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- My guess as to how to add MonadLog(ger)

import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Writer.Strict as Writer
import Say
import Test.Hspec
import Control.Monad.Identity (Identity(Identity))

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

class Monad m => MonadLog m where
  logSomething :: String -> m ()
instance (HasLog env, MonadIO m) => MonadLog (ReaderT env m) where
  logSomething msg = do
    log <- asks getLog
    liftIO (log msg)
-- instance Monad m => MonadLog (Writer.WriterT String m) where
--   logSomething = Writer.tell
instance MonadLog (Writer.Writer String) where
  logSomething = Writer.tell

modify :: MonadBalance m => (Int -> Int) -> m ()
modify = modifyBalance

main :: IO ()
main = hspec do
  describe "modify" do
    it "works, IO" do
      var <- newTVarIO (1 :: Int)
      runReaderT (modify (+ 2)) var
      res <- readTVarIO var
      res `shouldBe` 3
    it "works, pure" do
      let res = State.execState (modify (+ 2)) (1 :: Int)
      res `shouldBe` 3
  describe "logSomething" do
    it "works, IO" do
      var <- newTVarIO ""
      let logFunc msg = atomically do modifyTVar var (<> msg)
          msg1 = "Hello "
          msg2 = "World\n"
      runReaderT (logSomething msg1 *> logSomething msg2) logFunc
      res <- readTVarIO var
      res `shouldBe` (msg1 <> msg2)
    it "works, pure" do
      let msg1 = "Hello "
          msg2 = "World\n"
          res = Writer.execWriterT (logSomething msg1 *> logSomething msg2)
      res `shouldBe` Identity (msg1 <> msg2)
