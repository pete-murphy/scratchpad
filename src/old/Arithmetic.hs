module Arithmetic where

import Data.Function
-- >>> isPrime 2
-- True
isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | otherwise = not (foldr (\x acc -> acc || rem n x == 0) False [2..ceiling (sqrt (fromIntegral n))])

-- >>> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]
myGCD :: Int -> Int -> Int
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

-- >>> coprime 35 65
-- False
coprime :: Int -> Int -> Bool
coprime = (== 1) ... myGCD

-- blackbird :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) f g x y = f (g x y)

-- Euler's so-called totient function phi(m) is defined as the number of
-- positive integers r (1 <= r < m) that are coprime to m. 

-- >>> totient 10
-- 4

totient :: Int -> Int
totient n = length $ filter (coprime n) [1..(n-1)]

-- Determine the prime factors of a given positive integer. Construct a
-- flat list containing the prime factors in ascending order. 
-- primeFactors :: Int -> [Int]
-- primeFactors n
--   | n < 2 = []
--   -- if `n` is already prime, just return it in a list
--   | isPrime n = [n]
--   -- otherwise find the lowest prime that is a factor of `n`
--   | otherwise = 
--     let x = head (filter (\p -> isDivisible n p && isPrime p) [2..n])
--      in x : primeFactors (n `div` x)

-- isDivisible :: Int -> Int -> Bool
-- isDivisible divisor dividend = divisor `rem` dividend == 0

-- primeFactorsMult :: Int -> [(Int, Int)]
-- primeFactorsMult = toRLE . primeFactors

-- -- >>> primeFactorsMult 315
-- -- [(3,2),(5,1),(7,1)]

-- toRLE :: [Int] -> [(Int, Int)]
-- toRLE [] = []
-- toRLE ns@(n:_) = (n, length (takeWhile (== n) ns)) : toRLE (dropWhile (== n) ns)
