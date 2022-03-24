module LazyReverse where

data ReverseList a
  = Nil
  | ReverseList a :> a
  deriving (Show)

fromList :: [a] -> ReverseList a
fromList []     = Nil
fromList (a:as) = fromList as :> a

reverse' :: ReverseList a -> [a]
reverse' Nil       = []
reverse' (as :> a) = a : reverse' as
