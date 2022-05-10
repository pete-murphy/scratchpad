{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

import Control.Lens hiding (new)
-- Need this in scope to use generic lenses like '#someField'
import Data.Generics.Labels (Field')
import Data.Monoid (First)
import GHC.Generics

data List a = Nil | Cons a (List a) deriving (Show, Eq, Generic, Foldable)

-- Let’s say that we want to do some
-- programming for a school and need
-- to represent classes.
--
-- Classes have two components
-- - The class name
-- - The info for where and when to meet
--
-- Fortunately there’s already some
-- code that deals with meetings
-- so you just reuse that type
data Meeting = Meeting {time :: Int, room :: String}
  deriving (Generic)

-- And then you define you Class
-- type with it
data Class = Class {name :: String, meeting :: Meeting}
  deriving (Generic)

-- Your ticket is to add some logic
-- to class creation ensuring new classes
-- won’t be held in the same room as another
-- class at the same time. If there is
-- a conflict, we want to know the name
-- of the class it conflicts with.
--
-- Assuming you’ve been able to get a list of
-- all known classes from somewhere else
-- in the code, you implement the bit
-- of code to check for the conflicts
conflictingClass :: Class -> List Class -> Maybe String
conflictingClass newClass cs =
  case newClass of
    Class _ newMeeting -> case cs of
      Cons (Class otherName otherMeeting) others ->
        if newMeeting == otherMeeting
          then Just otherName
          else conflictingClass newClass others
      Nil -> Nothing

-- Everything looks like it’s working so you submit a PR
-- and get grumpy reviews saying that you should
-- be using this function defined in another module
conflictingMeeting :: HasMeeting a => a -> List a -> Maybe a
conflictingMeeting new as =
  case getMeeting new of
    Meeting newTime newRoom -> case as of
      Cons other others -> case getMeeting other of
        Meeting otherTime otherRoom ->
          if newTime == otherTime && newRoom == otherRoom
            then Just other
            else conflictingMeeting new others
      Nil -> Nothing

-- They suggest reivising your code to be this
conflictingClass2 :: Class -> List Class -> Maybe String
conflictingClass2 c = firstOf (folded . filteredBy (#meeting . only (c ^. #meeting)) . #name)

-- Unfortunately, this code doesn’t compile right
-- away because Class doesn’t implement HasMeeting.
-- You go look at the definition and see this.
class HasMeeting a where
  getMeeting :: a -> Meeting

-- Implement the instance (HasMeeting Class) below
-- in order to make the above code compile.
instance HasMeeting Class where
  getMeeting = view #meeting

-- For extra credit, address the other review comment
-- complaining that Meeting really should have an
-- Eq instance. Implement that instance and clean
-- up the code to use it.
instance Eq Meeting where
  Meeting n1 s1 == Meeting n2 s2 =
    n1 == n2 && s1 == s2

-- For further extra credit, help out the backend
-- architect and write a function similar to
-- conflictingMeeting, except it works for more
-- things than just stuff with a Meeting value inside.
-- conflictingBy :: Eq b => Lens' a b -> a -> List a -> Maybe a
-- conflictingBy l new = firstOf (folded . filteredBy (l . only (new ^. l)))
conflictingBy :: Eq b => a -> Lens' a b -> Fold (List a) a
conflictingBy new l = folded . filteredBy (l . only (new ^. l))

conflictingClass3 :: Class -> List Class -> Maybe String
conflictingClass3 class_ = firstOf ((class_ `conflictingBy` #meeting) . #name)
