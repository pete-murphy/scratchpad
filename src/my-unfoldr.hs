-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- myUnfoldr f x = f x >>= \(a, b) -> ([a] ++) `_` myUnfoldr f x

-- myUnfoldr f x = f x >>= \(a, b) -> (a :) <$> myUnfoldr f x

-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]

-- myUnfoldr f x = f x >>= \(a, b) -> ([a] ++) `_` myUnfoldr f x

-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- myUnfoldr f x = case f x of
--   Just (a, x') -> a : myUnfoldr f x'
--   Nothing -> []

myUnfoldr :: (b -> Maybe (a, b)) -> b -> Maybe [a]
myUnfoldr f x = f x >>= \(a, b) -> (a :) <$> myUnfoldr f b
-- >>> myUnfoldr (\x -> if x >= 10 then Nothing else Just (show x, x + 1)) 1
-- ProgressCancelledException

-- >>> myUnfoldr (\x -> if x >= 10 then Nothing else Just (show x, x * 3)) 1
-- ["1","3","9"]
