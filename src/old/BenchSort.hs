module BenchSort where

import Control.Monad (replicateM)
import Criterion.Main
import Data.List
import System.Random (randomRIO)

main :: IO ()
main = do
  rs <- replicateM 500 (randomRIO (0, 999)) :: IO [Int]
  defaultMain
    [ bench "maximum" $ whnf maximum rs,
      bench "take 1 . sort" $ whnf (take 1 . sort) rs,
      bench "sort" $ whnf sort rs
    ]
