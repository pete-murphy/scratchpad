module Nat where

data Nat
  = Zero
  | Succ Nat
  deriving (Show)

toInt :: Nat -> Integer
toInt Zero     = 0
toInt (Succ n) = 1 + toInt n

one = Succ Zero

two = Succ one

three = Succ two

four = Succ (Succ (Succ (Succ Zero)))

fromInt :: Integer -> Nat
fromInt 0 = Zero
fromInt n = Succ (fromInt (n - 1))

mkFour :: Nat
mkFour = go 0 Zero
  where
    go 4 n = n
    go m n = go (m + 1) (Succ n)

mkInfinity :: Nat
mkInfinity = go 0 Zero
  where
    go m n = go (m + 1) (Succ n)
