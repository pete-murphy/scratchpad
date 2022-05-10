data List a = Nil | Cons a (List a) deriving (Show, Eq)

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
data Meeting = Meeting Int String

-- And then you define you Class
-- type with it
data Class = Class String Meeting

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
    Class _ (Meeting newTime newRoom) -> case cs of
      Cons (Class otherName (Meeting otherTime otherRoom)) others ->
        if newTime == otherTime && newRoom == otherRoom
          then Just otherName
          else conflictingClass newClass others
      Nil -> Nothing

-- Everything looks like it’s working so you submit a PR
-- and get grumpy reviews saying that you should
-- be using this function defined in another module
conflictingMeeting :: HasMeeting a => a -> List a -> Maybe a
conflictingMeeting new as =
  case meeting new of
    Meeting newTime newRoom -> case as of
      Cons other others -> case meeting other of
        Meeting otherTime otherRoom ->
          if newTime == otherTime && newRoom == otherRoom
            then Just other
            else conflictingMeeting new others
      Nil -> Nothing

-- They suggest reivising your code to be this
conflictingClass2 :: Class -> List Class -> Maybe String
conflictingClass2 c cs =
  case conflictingMeeting c cs of
    Just (Class otherName _) -> Just otherName
    Nothing -> Nothing

-- Unfortunately, this code doesn’t compile right
-- away because Class doesn’t implement HasMeeting.
-- You go look at the definition and see this.
class HasMeeting a where
  meeting :: a -> Meeting

-- Implement the instance (HasMeeting Class) below

-- in order to make the above code compile.
instance HasMeeting Class where
  meeting (Class _name meeting) = meeting
-- For extra credit, address the other review comment
-- complaining that Meeting really should have an
-- Eq instance. Implement that instance and clean
-- up the code to use it.
-- For further extra credit, help out the backend
-- architect and write a function similar to
-- conflictingMeeting, except it works for more
-- things than just stuff with a Meeting value inside.
