module BasicTypeLevel where

data Unit =
  MkUnit

data Bool
  = True
  | False

data IntAndChar =
  MkIntAndChar Int Char

data HigherKinded f a
  = Bare a
  | Wrapped (f a)
