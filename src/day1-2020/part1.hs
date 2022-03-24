module Part1 where

import Control.Monad (guard)
import Data.List (tails)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

-- main :: IO Int
-- main = solve . mapMaybe readMaybe . lines <$> readFile "input"

solve :: [Int] -> [(Int, Int, Int)]
solve ns = do
  n : ms <- tails ns
  m : os <- tails ms
  o <- os
  guard (n + m + o == 2020)
  pure (n, m, o)

-- >>> solve [1721,979,366,299,675,1456]
-- [(979,366,675)]
