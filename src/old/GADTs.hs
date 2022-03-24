{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module GADTs where

import Control.Monad.Trans.State

data AnyIntOrBool a where
  AnyInt :: Int -> AnyIntOrBool Int
  AnyBool :: Bool -> AnyIntOrBool Bool
  AnyBoolToInt :: AnyIntOrBool Bool -> AnyIntOrBool Int

xs :: [AnyIntOrBool Int]
xs =
  [ AnyInt 1,
    AnyBoolToInt (AnyBool True)
  ]

-- data RemoteData x e a where
--   Initial :: forall e a. RemoteData A e a
--   Pending :: forall x e a. RemoteData x e a -> RemoteData B e a
--   Success :: forall e a. a -> RemoteData B e a -> RemoteData C e a
--   Failure :: forall e a. e -> RemoteData B e a -> RemoteData C e a


data A
data B

data ListLike x a where
  Nil :: ListLike A a
  Foo :: ListLike A a -> ListLike B a
  Bar :: a -> ListLike B a -> ListLike A a
  Baz :: b -> ListLike B a -> ListLike A a

x = Baz 0 $ Foo $ Bar "whatever" $ Foo Nil

y = Foo $ x

-- program :: forall m x. Monad m => StateT (RemoteData x String Int) m ()
-- program = do
--   put Initial
--   get >>= \x -> put $ Loading x
--   get >>= \x -> put $ Success () x
--   pure ()