{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

import Prelude hiding (map, sum)

fix :: (a -> a) -> a
fix f = let r = f r in r
-- fix f = f (fix f)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

map2 :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
map2 m _ [] = []
map2 m f (x : xs) = f x : m f xs

map3 :: (a -> b) -> [a] -> [b]
map3 = fix map2

------

type Fix :: (* -> *) -> *
data Fix f = Fix (f (Fix f))

fixMaybe :: Fix Maybe
fixMaybe = Fix (Just (Fix (Just (Fix Nothing))))

fixList :: Fix []
fixList = Fix [Fix [Fix []], Fix []]

data List2 r = Nil | Cons Int r
  deriving (Functor)

type List3 = Fix List2

list3 :: List3
list3 = Fix (Cons 1 (Fix (Cons 2 (Fix (Cons 3 (Fix Nil))))))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (Fix x) = alg (fmap (cata alg) x)

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

sum2 :: List2 Int -> Int
sum2 Nil = 0
sum2 (Cons n m) = n + m

sum3 :: List3 -> Int
sum3 = cata sum2

---

-- I asked what the significance of the Functor constraint was in `cata`
-- and Sam showed that we don't _actually_ need Functor if we define it
-- in the Mendler(?) style
cata' :: (forall x. f x -> (x -> a) -> a) -> Fix f -> a
cata' alg (Fix z) = alg z (cata' alg)

newtype Coyoneda f a = Coyoneda (forall b. (a -> b) -> f b)