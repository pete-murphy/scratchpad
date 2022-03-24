module ShowInt where

import Data.Char (chr)

showInt :: Int -> String
showInt x = go (floor $ realToFrac $ logBase 10 $ fromIntegral x) x
  where
    go place x
      | x <= 0 = []
      | otherwise =
        let digit = x `div` 10 ^ place
         in chr (digit + 48) : go (place - 1) (x - (digit * 10 ^ place))
