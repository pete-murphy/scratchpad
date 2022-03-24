module Split where

split y = go []
  where
    go buffer [] = [buffer]
    go buffer (x : xs)
      | x == y = buffer : go [] (dropWhile (== ' ') xs)
      | otherwise = go (buffer ++ [x]) xs
