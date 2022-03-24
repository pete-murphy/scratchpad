module NinetyNineProblems where

import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

myLast :: [a] -> a
myLast [] = undefined
myLast (x : []) = x
myLast (_ : xs) = myLast xs

myLast' :: [a] -> a
myLast' list = case list of
  (x : []) -> x
  (_ : xs) -> myLast' xs

myButLast :: [a] -> a
myButLast [] = undefined
myButLast (x : _ : []) = x
myButLast (_ : xs) = myButLast xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

data Option a = None | Some a deriving (Eq, Show)

elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x : _) 1 = Just x
elementAt (_ : rest) n = elementAt rest (n -1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == myReverse list

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (xs : xss)) = flatten xs ++ flatten (List xss)

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : xs) = x : compress (dropWhile ((==) x) xs)

-- How we originally had it:
compress' :: Eq a => [a] -> [a]
compress' [] = []
compress' (x : []) = [x]
compress' (x : y : xs) =
  -- compare x y
  if x /= y
    then -- if x is not equal y then keep x and compress y:xs
      x : compress' (y : xs)
    else -- else discard x and recurse
      compress' (y : xs)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x : xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)

------------------------------------------------------------------- 28

lsort :: [[a]] -> [[a]]
-- lsort a = unTuplify (sortTuples (pairWithLength a))
-- lsort = unTuplify . sortTuples . pairWithLength
lsort = unTuplify . sortTuples . pairWithLength

lsort2 a = a |> pairWithLength |> sortTuples |> unTuplify

pairWithLength :: [[a]] -> [(Int, [a])]
pairWithLength = fmap (\x -> (length x, x))

sortTuples :: [(Int, [a])] -> [(Int, [a])]
sortTuples = sortOn fst

unTuplify :: [(Int, [a])] -> [[a]]
unTuplify = fmap snd

x |> f = f x

-- >>> lsort2 ["abc","de","fgh","de","ijkl","ab"]
-- ["de","de","ab","abc","fgh","ijkl"]

-- b) Again, we suppose that a list contains elements that are lists themselves.
-- But this time the objective is to sort the elements of this list according to
-- their length frequency; i.e., in the default, where sorting is done
-- ascendingly, lists with rare lengths are placed first, others with a more
-- frequent length come later.

-- >>> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]

-- Expected result: ["ijkl","o","abc","fgh","de","de","mn"]

type Length = Int

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort2 . reverse . groupByLength

groupByLength :: [[a]] -> [[[a]]]
groupByLength xs = foldr go [] xs
  where
    go :: [a] -> [[[a]]] -> [[[a]]]
    go as [] = [[as]]
    -- go (l, as) ([] : aseses) = go (l, as) aseses
    go as (ases'@(as' : _) : aseses) =
      if length as' == length as
        then (as : ases') : aseses
        else ases' : (go as aseses)
