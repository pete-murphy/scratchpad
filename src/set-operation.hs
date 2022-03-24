import Data.Foldable (traverse_)
import qualified Data.Set as S
import Data.Set (Set)

traverseTwoSets_ ::
  (Ord a, Applicative f) =>
  Set a ->
  Set a ->
  (a -> f b) ->
  (a -> f c) ->
  (a -> f d) ->
  f ()
traverseTwoSets_ leftSet rightSet onLeft onRight onBoth =
  traverse_ onLeft (leftSet `S.difference` rightSet)
    *> traverse_ onRight (rightSet `S.difference` leftSet)
    *> traverse_ onBoth (leftSet `S.intersection` rightSet)

main = do
  let l = S.fromList [1, 2, 3]
      r = S.fromList [3, 4, 5]
      onLeft x = print ("Left: " <> show x)
      onRight x = print ("Right: " <> show x)
      onBoth x = print ("Both: " <> show x)
  traverseTwoSets_ l r onLeft onRight onBoth
