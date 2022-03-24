import Data.List

lsort :: [[a]] -> [[a]]
-- lsort a = unTuplify (sortTuples (pairWithLength a))
-- lsort = unTuplify . sortTuples . pairWithLength
lsort = unTuplify . sortTuples . pairWithLength

lsort2 a = a |> pairWithLength |> sortTuples |> unTuplify

pairWithLength :: [[a]] -> [(Int, [a])]
pairWithLength = fmap (\x -> (length x, x))

sortTuples :: [(Int, [a])] -> [(Int, [a])]
sortTuples = sortOn fst

unTuplify :: [(Int, [a])] -> [[a]]
unTuplify = fmap snd

x |> f = f x

-- >>> lsort2 ["abc","de","fgh","de","ijkl","ab"]
-- ["de","de","ab","abc","fgh","ijkl"]

-- b) Again, we suppose that a list contains elements that are lists themselves.
-- But this time the objective is to sort the elements of this list according to
-- their length frequency; i.e., in the default, where sorting is done
-- ascendingly, lists with rare lengths are placed first, others with a more
-- frequent length come later.

-- >>> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]

-- Expected result: ["ijkl","o","abc","fgh","de","de","mn"]

type Length = Int

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort2 . reverse . groupByLength

groupByLength :: [[a]] -> [[[a]]]
groupByLength xs = foldr go [] xs
  where
    go :: [a] -> [[[a]]] -> [[[a]]]
    go as [] = [[as]]
    -- go (l, as) ([] : aseses) = go (l, as) aseses
    go as (ases'@(as' : _) : aseses) =
      if length as' == length as
        then (as : ases') : aseses
        else ases' : (go as aseses)
