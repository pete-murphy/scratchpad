{-# LANGUAGE ViewPatterns #-}
module PMExample where

import Data.Ord (comparing, Down(Down))
import Data.List (tails, sort, group, sortOn, sortBy)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (listToMaybe)
import Control.Monad (guard)

missingValues =
  let first = (*2) . head
      lasty = last
  in  take 2 . sortBy (comparing Down) $ [1, 1, 1, 2, 2, 3]

-- >>> missingValues
-- [3,2]

-- testingOthersSolutionA :: Integer
-- testingOthersSolutionA = f . sortOn length . group . sort $ [1,1,1,2,2,3]

f listOfLists = 
   let [x] = listOfLists !! 0 
       [y,_] = listOfLists !! 1
    in x * x * y

-- >>> testingOthersSolutionA
-- 6

testingOthersSolutionD = f . sortOn length . group . sort $ [1,1,1,2,2,2,3]
  where f ([x] : [y , _] : _) = x * x * y;

testingOthersSolutionB = f . sortOn length . group . sort $ [1,1,1,2,3]
  where f ([x] : [y , _] : _) = x * x * y;
testingOthersSolutionC = f . sortOn length . group . sort $ [1,1,1,2,2,3,3]
  where f ([x] : [y , _] : _) = x * x * y;

-- >>> testingOthersSolutionB
-- /Users/peter/Code/haskell-beginners/scratchpad/src/PMExample.hs:29:9-41: Non-exhaustive patterns in function f

foo = (\((x:_):(y:_):_) -> x * x * y) . group . sortOn Down

-- >>> foo [1,1,1,2,2,3]
-- 18

-- buy :: Int -> [Int] -> Maybe (Int, Int)
-- buy n (zip [0..] -> xs) = listToMaybe [(i,j) | ((i,y):ys) <- tails xs, (j,z) <- ys, y + z == n ]

-- >>> buy 2 [1,1]
-- Just (0,1)

      --  = Just (0,1)
-- >>> buy 3 [1,1]
-- Nothing

      --  = Nothing
-- >>> buy 5 [5,2,3,4,5]
-- Just (1,2)

--  = Just (1,2)

-- buy' n ((i,x):xs) = [(i,j) | ts@((j,y):_) <- tails xs, , y + z == n]
-- buy n 

buy n xs = listToMaybe [(i,j) | ((i,y):ys) <- tails (zip [0..] xs), (j,z) <- ys, y + z == n ]

buy_ :: Int -> [Int] -> Maybe (Int, Int)
buy_ c is = pairFinder c sIs (reverse sIs) 0 (length is - 1)
   where
     sIs = sort is
pairFinder :: Int -> [Int] -> [Int] -> Int -> Int -> Maybe (Int, Int)
pairFinder c (x:xs) (y:ys) xInd yInd
    | xInd >= yInd = Nothing
    | x + y == c = Just (xInd, yInd)
    | x + y < c = pairFinder c xs (y:ys) (xInd + 1) yInd
    | otherwise = pairFinder c (x:xs) ys xInd (yInd - 1)

-- >>> :set +s
-- >>> buy_ 5 ([5,2,3,4,5] ++ [1..1000000])
-- unknown command 'set'
