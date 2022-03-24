module Beginners where

-- >>> combinations 3 "abcd"
-- ["abc","abd","acd","bcd"]
combinations :: Show a => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n list@ ~(x : xs)
  | length list < n = []
  | otherwise =
    -- combinations that include x
    map (x :) (combinations (n -1) xs)
      -- combinations that don't
      <> combinations n xs
