{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}
-- import Control.Monad.Trans.State as State
import Control.Monad.State as State
import Data.Traversable (mapAccumL)

arrayA :: [Int]
arrayA = [9, 5, 4, 7]

arrayB :: [Int]
arrayB = [3, 2, 4, 5, 2, 1, 1, 3, 4]

expectedOutput :: [[Int]]
expectedOutput = [[3, 2, 4], [5], [2, 1, 1], [3, 4]]

-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- traverse :: (t ~ [], f ~ State [Int]) => (a -> List [Int] b) -> [a] -> State [Int] [b]
-- traverse :: (t ~ [], f ~ State [Int]) => (Int -> List [[Int]] Int) -> [Int] -> State [[Int]] [Int]
-- traverse :: (t ~ [], f ~ State [Int]) => (Int -> [[Int]] -> (Int, [[Int]])) -> [Int] -> State [[Int]] [Int]

-- foo :: [Int] -> [Int] -> [[Int]]
-- foo bs as = traverse f as
--  where
--  f :: Int -> [Int]
--  f n = flip State.execState [] $ State.state \ms -> go n 0 ms []
--                                                     where go n' x (m:ms) acc | x >= n' = (acc, m:ms)
--                                                                              | otherwise =

consumeUntilSumIs :: Int -> [Int] -> ([Int], [Int])
consumeUntilSumIs n = go []
  where
    go :: [Int] -> [Int] -> ([Int], [Int])
    go acc [] = (acc, [])
    go acc unconsumed@(x : xs)
      | sum acc >= n = (acc, unconsumed)
      | otherwise = go (acc <> [x]) xs

groupByMatchingTotals :: [Int] -> [Int] -> [[Int]]
groupByMatchingTotals [] _ = []
groupByMatchingTotals (n : ns) xs = let (c, u) = consumeUntilSumIs n xs in c : groupByMatchingTotals ns u

-- |
-- foo :: [Int] -> [Int] -> [[Int]]
-- foo bs as = traverse f as
--   where f :: Int -> [Int]
--         f

-- foo :: [Int] -> [Int] -> [[Int]]
-- foo bs as = fst $ mapAccumL _ [] as

-- foo :: [Int] -> [Int] -> [[Int]]
-- foo bs as = snd $ mapAccumL f [] as
--   where
--     f' :: [Int] -> Int -> ([Int], [Int])
--     f' xs n = ([1], xs)
--     f :: [Int] -> Int -> ([Int], [Int])
--     f xs n = go [] xs
--       where
--         go acc [] = (acc, [])
--         go acc (y : ys)
--           | sum acc >= n = (acc, y : ys)
--           | otherwise = go (acc <> [y]) ys
