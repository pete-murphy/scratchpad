data Thingy
  = Yak Int
  | Breeze Color Bool
  | Cabinet Thingy (List Bool)
  | Can (List (Maybe Thingy))
  deriving (Show)

data Color = Red | Blue | Green deriving (Show)

data List a = Nil | Cons a (List a) deriving (Show)

-- Purpose: given a Thingy, produce the sum of yaks in a thingy
homework :: Thingy -> Int
homework thingy = case thingy of
  Yak n -> n
  Breeze _ _ -> 0
  Cabinet t _ -> homework t
  Can maybeThingys -> listOfThingysToInt (compact maybeThingys)
  where
    listOfThingysToInt :: List Thingy -> Int
    listOfThingysToInt = foldList (\thingy n -> homework thingy + n) 0

-- >>> thingy = Can (Cons (Just (Yak 0)) (Cons (Just (Yak 1)) (Cons Nothing (Cons Nothing Nil))))
-- >>> homework thingy
-- 1

compact :: List (Maybe a) -> List a
compact maybeThingys = case maybeThingys of
  Nil -> Nil
  Cons mt mts -> case mt of
    Nothing -> compact mts
    Just th -> Cons th (compact mts)

example :: List (Maybe Thingy)
example = Cons (Just (Yak 0)) (Cons Nothing (Cons (Just (Yak 1)) Nil))

-- >>> helperFn example
-- Cons (Yak 0) (Cons (Yak 1) Nil)

foldList :: (a -> b -> b) -> b -> List a -> b
foldList f acc list = case list of
  Nil -> acc
  -- Cons x xs -> f x (foldList f acc xs)
  Cons x xs -> foldList f (f x acc) xs

-- Could also do this:
-- Cons th ths -> fold f (f th acc) ths

filterList :: (a -> Bool) -> List a -> List a
filterList = undefined

mapList :: (a -> b) -> List a -> List b
mapList = undefined

appendList :: List a -> List a -> List a
appendList xs ys = foldList Cons ys (foldList Cons Nil xs)

-- >>> appendList (toList [1..5]) (toList [10..20])
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 10 (Cons 11 (Cons 12 (Cons 13 (Cons 14 (Cons 15 (Cons 16 (Cons 17 (Cons 18 (Cons 19 (Cons 20 Nil)))))))))))))))

-- Bonus?
fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList = foldr Cons Nil
