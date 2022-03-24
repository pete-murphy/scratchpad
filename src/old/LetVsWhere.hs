module LetVsWhere where

findFirstEven :: [Int] -> Maybe Int
findFirstEven [] = Nothing
findFirstEven (n:ns)
  | isEven n = Just n
  | otherwise = findFirstEven ns
  where
    isEven m = mod m 2 == 0
