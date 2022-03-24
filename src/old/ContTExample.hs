{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ContTExample where

newtype ContT r m a =
  ContT
    { runContT :: (a -> m r) -> m r
    }

newtype Cont r a =
  Cont
    { runCont :: (a -> r) -> r
    }

-- Guessing as to how ContT might be used
foo'' :: Int -> (Int -> IO ()) -> IO ()
foo'' n callback = do
  callback n
  putStrLn $ "from foo: " ++ show n

foo' :: Int -> ContT () IO Int
foo' n =
  ContT $ \callback -> do
    callback n
    putStrLn $ "from foo': " ++ show n

callback :: Int -> IO ()
callback n = putStrLn $ "from callback: " ++ show n

-- bindCont :: Cont r a -> (a -> Cont r b) -> Cont r b
-- bindCont (Cont aToRToR) aToContRB =
--   Cont $ (\bToR -> aToRToR ((flip $ runCont . aToContRB) bToR))
instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f (Cont ra) = Cont $ \br -> ra (br . f)

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure a = Cont $ \ar -> ar a
  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  --       (((a -> b) -> r) -> r) -> ((a -> r) -> r) -> (b -> r) -> r
  rab <*> ra = rab >>= \f -> f <$> ra
  -- Cont rab <*> Cont ra = Cont $ \

--   rab <*> ra = do
--     f <- rab
--     f <$> ra
instance Monad (Cont r) where
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  Cont ra >>= f = Cont $ ra . flip (runCont . f)
-- (>>=) :: Monad m       => m a -> (a -> m b) -> m b
-- (<*>) :: Applicative m => m (a -> b) -> m a -> m b
-- (=<<) :: Monad m       => (a -> m b) -> m a -> m b
