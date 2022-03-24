{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

data Foo = Some Int | None

instance Semigroup Foo where
  Some n <> Some m = Some (n + m)
  Some n <> None = Some n
  None <> Some n = Some n
  None <> None = None

instance Monoid Foo where
  mempty = None

newtype Bar = Bar Foo

instance Semigroup Bar where
  Bar (Some n) <> Bar (Some m) = Bar (Some (n * m))
  Bar (Some n) <> Bar None = Bar (Some n)
  Bar None <> Bar (Some n) = Bar (Some n)
  Bar None <> Bar None = Bar None

instance Monoid Bar where
  mempty = Bar None

-----------

newtype FnFoo a = FnFoo (a -> Foo)
  deriving (Semigroup, Monoid)

newtype FnBar a = FnBar (a -> Foo)
  deriving (Semigroup, Monoid) via (a -> Bar)

rowsToIntList :: String -> IO [Int]
rowsToIntList = undefined

data Increased = Increased | Decreased
  deriving (Eq, Show)

getLargerMesurement :: [Int] -> [(Int, Increased)]
getLargerMesurement = undefined

main :: IO ()
main = do
  entries <- rowsToIntList "./data/input0"
  let displayableCount =
        show
          $ length
          $ filter (\x -> snd x == Increased)
          $ getLargerMesurement entries
  putStrLn displayableCount
