{-# LANGUAGE DeriveFunctor #-}

import Data.Foldable
import Data.Monoid

data Color = Red | Green | Blue deriving (Show)

data List a = Nil | Cons a (List a) deriving (Show)

data Stuff a
  = Buffalo a
  | Wind Color Bool
  | Drawer (Stuff a) (List Bool)
  | Cant (List (Maybe (Stuff a)))
  deriving (Show, Functor)

-- instance Functor Stuff where
--   fmap f (Buffalo a) = Buffalo (f a)
--   fmap f (Drawer s bs) = Drawer (fmap f s) bs
--   fmap f (Cant mss) = Cant (fmap (fmap (fmap f)) mss)
--   fmap f (Wind c b) = Wind c b

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ Nil = Nil

instance Foldable List where
  foldr f b (Cons x xs) = f x (foldr f b xs)
  foldr _ b Nil = b

instance Foldable Stuff where
  foldMap f (Buffalo x) = f x
  foldMap f (Wind _ _) = mempty
  foldMap f (Drawer stuff _) = foldMap f stuff
  foldMap f (Cant xs) = (foldMap . foldMap . foldMap) f xs

-- extraCredit2 :: Monoid a => Stuff a -> a
-- extraCredit2 = fold

-- Implement this
extraCredit1 :: Stuff Int -> Int
extraCredit1 = getSum . foldMap Sum

-- What needs to change if we don't know the values in Stuff?
-- HINT: This compiles, but you won't be able to implement it as is. You'll need more inputs to make this work
extraCredit2 :: Monoid a => Stuff a -> a
extraCredit2 = fold

-- >>> extraCredit2 stuff4

stuff1 :: Stuff Int
stuff1 = Buffalo 20

-- 20

stuff2 :: Stuff Int
stuff2 = Cant (Cons (Just (Buffalo 5)) (Cons (Just (Buffalo 2)) (Cons (Just (Wind Red True)) (Cons (Just (Drawer stuff1 Nil)) Nil))))

-- 27

stuff3 :: Stuff Int
stuff3 = Cant (Cons (Just stuff3) Nil)

-- Never ends
stuff4 :: Stuff String
stuff4 = Cant (Cons (Just (Buffalo "hello")) (Cons (Just (Buffalo "world")) (Cons (Just (Wind Red True)) (Cons (Just (Drawer (Buffalo "!") Nil)) Nil))))
