{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}
 
import Control.Monad.Cont (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, Except)
import Data.Foldable (Foldable (foldMap), foldlM)
import Data.Monoid
import RIO.Prelude hiding (log)
import RIO.Prelude.Types
import Data.Kind
import Prelude (print)

-- foldMapM ::
--   (Monad m, Monoid w, Foldable t) =>
--   (a -> m w) ->
--   t a ->
--   m w
-- foldMapM f =
--   foldlM
--     ( \acc a -> do
--         w <- f a
--         return $! mappend acc w
--     )
--     mempty

-- foldMapA f = getAp . foldMap (Ap . f)

-- >>> foldMapM (\x -> if odd x then Just [show x] else Nothing) [1,3,5]
-- Just ["1","3","5"]

-- >>> foldMapA (\x -> if odd x then Just [show x] else Nothing) [1,3,5]
-- Just ["1","3","5"]
  -- = ReaderTaskEither {runReaderTaskEither ::  ReaderT r (ExceptT e IO) a}
  -- = ReaderTaskEither {runReaderTaskEither :: 


newtype ReaderTaskEither r e a
  = ReaderTaskEither {runReaderTaskEither :: ReaderT r (ExceptT e IO) a}

type Wat
  :: ((Type -> Type) -> Type -> Type)
  -> (Type -> Type -> Type)
  -> Type -> Type -> Type
data Wat r m x y where
  Wat :: Wat r (m x) y z

deriving instance Bifunctor m => Bifunctor (Wat (ReaderT r) m) 
deriving instance Bifunctor m => Bifunctor (Wat (ExceptT e) m) 

deriving instance Functor (ReaderTaskEither r e)
deriving instance Applicative (ReaderTaskEither r e)
deriving instance Bifunctor (ReaderTaskEither r)

------------ ////// -- >>> import System.IO.Unsafe (unsafePerformIO)






-- foo :: String

-- >>> ReaderTaskEither x = bimap show show (pure 9 :: ReaderTaskEither () () Int)
-- >>> runExceptT (runReaderT x ())







type Flip :: forall k. (Type -> (Type -> Type) -> k) -> (Type -> Type) -> Type -> k
type Flip f g t = f t g

-- deriving instance Functor (ReaderTaskEither r e)
-- deriving instance Foldable (ReaderTaskEither r e)



-- newtype Flip p a b = Flip { runFlip :: p b a }

--
-- deriving instance Bifoldable (ReaderTaskEither r)

--   bimap f g (runReaderTaskEither -> runReaderT -> fmap runExceptT -> rea) =
--     ReaderTaskEither
--       ( ReaderT \r ->
--           ExceptT (bimap f g <$> rea r)
--       )

-- instance Bifunctor (ReaderTaskEither r) where
--   bimap :: (e -> e') -> (a -> a') -> ReaderTaskEither r e a -> ReaderTaskEither r e' a'
--   bimap f g (ReaderTaskEither (ReaderT rea)) = ReaderTaskEither $
--     ReaderT \r ->
--       let ExceptT ea = rea r
--        in ExceptT (bimap f g <$> ea)

-- instance Bifoldable (ReaderTaskEither r) where
--   bimap :: (e -> e') -> (a -> a') -> ReaderTaskEither r e a -> ReaderTaskEither r e' a'
--   bimap f g (ReaderTaskEither (ReaderT rea)) = ReaderTaskEither do
--     ReaderT \r -> do
--       let ExceptT ea = rea r
--        in ExceptT (bimap f g <$> ea)

newtype ApiClient
  = ApiClient
      { apiClient :: ()
      }

data Error

data User
  = User
      { key :: String,
        name :: String,
        bio :: String,
        age :: Int
      }

-- getUserKeys :: ReaderTaskEither ApiClient Error [String]
-- getUserKeys = pure []

-- getUsersByKey :: [String] -> ReaderTaskEither ApiClient Error [User]
-- getUsersByKey _ = pure []

-- log :: Show a => a -> IO ()
-- log = print

-- warn :: Show a => a -> IO ()
-- warn = print

-- myProgram :: ReaderTaskEither ApiClient Error [User]
-- myProgram = do
--   keys <- getUserKeys
--   liftIO $ bitraverse_ _ _ (Left "foo")
--   getUsersByKey keys

reduceF :: (a -> b) -> ((a -> b) -> c) -> c
reduceF ab abc = abc ab