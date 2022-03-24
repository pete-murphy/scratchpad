module IsPrime where

isPrime :: Int -> Bool
isPrime x
  | x <= 1 = False
  | elem 0 [x `mod` y | y <- [2 .. floor $ sqrt $ fromIntegral x]] = False
  | otherwise = True

--   | x `mod` y == 0 for y in [2, 3 .. floor x^0.5] = False
main :: IO ()
main = do
  print $ isPrime 2
  print $ isPrime 3
  print $ isPrime 9
  print $ isPrime 11
