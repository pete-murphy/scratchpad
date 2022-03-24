module CamelCase where

import           Control.Arrow
import           Data.Char     (toUpper)

toCaps :: String -> String
toCaps []         = []
toCaps (' ':c:cs) = toUpper c : toCaps cs
toCaps (c:cs)     = c : toCaps cs

toCaps' :: String -> String
toCaps' =
  concat .
  uncurry (++) .
  fmap ((uncurry (++) . first (toUpper <$>) . splitAt 1) <$>) .
  splitAt 1 . words

foo :: Int -> (([a], [a]) -> ([a], [a])) -> [a] -> [a]
foo n f = uncurry (++) . f . splitAt n

camelCase :: String -> String
camelCase = concat . foo 1 (fmap $ fmap $ foo 1 $ first (toUpper <$>)) . words

toCaps'' :: String -> String
toCaps'' = flip (foldr go (const [])) id
  where
    go ' ' acc _ = acc toUpper
    go x acc f   = f x : acc id
