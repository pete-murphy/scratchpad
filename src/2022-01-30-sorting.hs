{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Data.List (sortBy)
import qualified Data.List as List
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import GHC.Base (Symbol)
import GHC.OverloadedLabels (IsLabel (..))

data Person
  = Person
      { name :: String,
        age :: Int,
        likesDogs :: Bool
      }
  deriving (Show)

class ComparesPerson (label :: Symbol) where
  comparison :: Person -> Person -> Ordering

  -- Really just want an ordered set of functions, but need to be able to
  -- compare for equality so using a string :(
  toString :: String

data SortPerson where
  CP ::
    forall (a :: Symbol).
    ComparesPerson a =>
    Proxy a ->
    SortPerson

instance Eq SortPerson where
  CP (Proxy :: Proxy x) == CP (Proxy :: Proxy y) =
    toString @x == toString @y

instance ComparesPerson "likesDogs" where
  comparison = comparing likesDogs
  toString = "likesDogs"

instance ComparesPerson "age" where
  comparison = comparing age
  toString = "age"

instance ComparesPerson "name" where
  comparison = comparing name
  toString = "name"

-- List of sorting methods that user can change (either by moving a method
-- around in the list/ordered-set, like if they wanted to sort by name first),
-- or remove from list
sortingMethods :: [SortPerson]
sortingMethods =
  [ CP (Proxy @"likesDogs"),
    CP (Proxy @"age"),
    CP (Proxy @"name")
  ]

people :: [Person]
people =
  [ Person "Alice" 30 True,
    Person "Alice" 40 True,
    Person "Bob" 40 True,
    Person "Bob" 40 False,
    Person "Carol" 20 True
  ]

comparisonFrom :: [SortPerson] -> Person -> Person -> Ordering
comparisonFrom = foldMap \(CP (Proxy :: Proxy x)) -> comparison @x

main :: IO ()
main =
  print (sortBy (comparisonFrom sortingMethods) people)
