import Control.Arrow
import qualified Data.Set as Set
import Data.Tuple

findOdd' :: [Int] -> Int
findOdd' =
  Set.findMin
    . foldl (\acc x -> if Set.member x acc then Set.delete x acc else Set.insert x acc) Set.empty

findOdd :: [Int] -> Int
findOdd =
  Set.findMin
    . foldr (Set.union . curry . (Set.difference *** Set.difference . swap) . uncurry) Set.empty
