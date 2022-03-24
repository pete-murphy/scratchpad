module Zellers where

data Date =
  Date
    { q :: Int
    , m :: Int
    , j :: Int
    , k :: Int
    }
  deriving (Show)

fromString :: String -> Maybe Date
fromString (d1:d2:'-':m1:m2:'-':ys) =
  if (m > 2)
    then Just $ Date q m j k
    else Just $ Date q (m + 12) j k
  where
    q = read [d1, d2]
    m = read [m1, m2]
    (j, k)
      | m > 2 = divMod (read ys) 100
      | otherwise = divMod (read ys - 1) 100
fromString _ = Nothing

zellers :: Date -> Int
zellers (Date q m j k) =
  flip mod 7 $ q + (div (13 * (m + 1)) 5) + k + (div k 4) + (div j 4) - (2 * j)

main :: IO ()
main = do
  print $ zellers <$> fromString "06-10-2019" -- "Just 1" (Sunday)
  print $ zellers <$> fromString "15-06-2019" -- "Just 0" (Saturday)
  print $ zellers <$> fromString "01-01-2019" -- "Just 3" (Tuesday)
