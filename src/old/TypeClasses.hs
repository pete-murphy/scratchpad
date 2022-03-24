module TypeClasses where

import           Data.Foldable (foldl')
import           Prelude       hiding (Monoid, foldMap)

newtype Printable' a =
  Printable' (a -> String)

-- _equals :: Equal' -> (a -> a -> Bool)
newtype Equal' a =
  Equal'
    { equals' :: a -> a -> Bool
    }

logValue' :: a -> Printable' a -> IO ()
logValue' a (Printable' p) = putStrLn (p a)

class Printable a where
  print' :: a -> String

class Equal a where
  equals :: a -> a -> Bool

logValue :: Printable a => a -> IO ()
logValue a = putStrLn (print' a)

data User =
  User
    { userId   :: Int
    , userName :: String
    }
  deriving (Show)

instance Equal Int where
  equals = (==)

instance Equal Char where
  equals = (==)

instance (Equal a) => Equal [a] where
  equals as as' = length as == length as' && (and $ zipWith equals as as')

instance Equal User where
  User id1 name1 `equals` User id2 name2 =
    id1 `equals` id2 && name1 `equals` name2

elem' :: Equal a => a -> [a] -> Bool
elem' _ []     = False
elem' a (x:xs) = a `equals` x || elem' a xs

removeDupes :: Equal a => [a] -> [a]
removeDupes []     = []
removeDupes (u:us) = u : removeDupes (filter (not . equals u) us)

users :: [User]
users =
  [ User {userId = 1, userName = "Bob"}
  , User {userId = 2, userName = "Susan"}
  , User {userId = 1, userName = "Bob"}
  , User {userId = 3, userName = "Carol"}
  , User {userId = 3, userName = "Carol"}
  , User {userId = 3, userName = "Carol"}
  , User {userId = 3, userName = "Carol"}
  ]

data Point a b =
  Point a b
  deriving (Eq)

data Foo =
  Foo
  deriving (Eq)

-- interface Monoid<A> {
--   empty: A;
--   concat: (x: A, y: A) => A;
-- }
data Monoid' a =
  Monoid'
    { empty'  :: a
    , append' :: a -> a -> a
    }

monoidList :: Monoid' [a]
monoidList = Monoid' [] (++)

monoidSum :: Monoid' Int
monoidSum = Monoid' 0 (+)

monoidProduct :: Monoid' Int
monoidProduct = Monoid' 1 (*)

foldMap :: Monoid' b -> (a -> b) -> [a] -> b
foldMap (Monoid' e app) f = foldl' (flip (app . f)) e

-- foldMap :: Monoid' b -> (a -> b) -> [a] -> b
-- foldMap (Monoid' e app) f as = case as of
--   [] -> e
--   (a:as') -> app (f a) (foldMap (Monoid' e app) f as')

class Monoid a where
  empty :: a
  append :: a -> a -> a

instance Monoid [a] where
  empty = []
  append = (++)

newtype And =
  And Bool

newtype Or =
  Or Bool

instance Monoid And where
  append (And b) (And b') = And (b && b')
  empty = And True

instance Monoid Or where
  append (Or b) (Or b') = Or (b || b')
  empty = Or False

main :: IO ()
main = do
  -- Print the unique users
  print $ removeDupes users
  -- Below doesn't work without the Eq instance for Foo
  print $ Point Foo 9 == Point Foo 12
