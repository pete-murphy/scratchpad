import qualified Data.Interval as Interval
import Data.Interval ((<=..<), Interval)
import qualified Data.IntervalMap.Lazy as IntervalMap
import Data.IntervalMap.Lazy (IntervalMap)

type EventKey = String

data ResourceAllocated = ResourceAllocated {key :: EventKey, lo :: Int, hi :: Int}
  deriving (Show)

existingMap :: IntervalMap Int [ResourceAllocated]
existingMap = IntervalMap.singleton (0 <=..< 10) [ResourceAllocated "foo" 0 20]

newMap :: IntervalMap Int [ResourceAllocated]
newMap = IntervalMap.insertWith f (5 <=..< 20) [ResourceAllocated "bar" 0 10] existingMap
  where
    f :: [ResourceAllocated] -> [ResourceAllocated] -> [ResourceAllocated]
    f [ResourceAllocated k l h] old =
      let maxOldHi = maximum (hi <$> old)
       in ResourceAllocated k (l + maxOldHi) (h + maxOldHi) : old

-- | Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@ will insert the pair (interval, value) into @mp@.
-- If the interval overlaps with existing entries, the value for the entry is replace
-- with @(f new_value old_value)@.
insertWith :: Ord k => (a -> a -> a) -> Interval k -> a -> IntervalMap k a -> IntervalMap k a
insertWith f i a m =
  if Interval.null i
    then m
    else alter g i m
  where
    g = Just (maybe a (f a))

-- insertWith _ i _ m | Interval.null i = m
-- insertWith f i a m = alter g i m
--   where
--     g Nothing = Just a
--     g (Just a') = Just $ f a a'

-- alter :: (Maybe a -> Maybe a) -> Interval k -> IntervalMap k a -> IntervalMap k a
-- | The expression (@'alter' f i map@) alters the value @x@ at @i@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'IntervalMap'.
-- alter :: Ord k => (Maybe a -> Maybe a) -> Interval k -> IntervalMap k a -> IntervalMap k a
-- alter _ i m | Interval.null i = m
-- alter f i m =
--   case split i m of
--     (IntervalMap m1, IntervalMap m2, IntervalMap m3) ->
--       let m2' = Map.mapMaybe (\(j,a) -> (\b -> (j,b)) <$> f (Just a)) m2
--           js = IntervalSet.singleton i `IntervalSet.difference` keysSet (IntervalMap m2)
--           IntervalMap m2'' =
--             case f Nothing of
--               Nothing -> empty
--               Just a -> fromList [(j,a) | j <- IntervalSet.toList js]
--       in IntervalMap $ Map.unions [m1, m2', m2'', m3]
