data List a = Nil | Cons a (List a) deriving (Show, Eq)

-- Let's say that we want to do some
-- programming for a school and need
-- to represent classes.
--
-- Classes have two components
-- - The class name
-- - The info for where and when to meet
--
-- Fortunately there's already some
-- code that deals with meetings
-- so you just reuse that type
data Meeting = Meeting Int String deriving (Show)

-- And then you define you Class
-- type with it
data Class = Class String Meeting deriving (Show)

-- Your ticket is to add some logic
-- to class creation ensuring new classes
-- won't be held in the same room as another
-- class at the same time. If there is
-- a conflict, we want to know the name
-- of the class it conflicts with.
--
-- Assuming you've been able to get a list of
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

-- Everything looks like it's working so you submit a PR
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

-- Unfortunately, this code doesn't compile right
-- away because Class doesn't implement HasMeeting.
-- You go look at the definition and see this.
class HasMeeting a where
  meeting :: a -> Meeting

-- Implement the instance (HasMeeting Class) below
instance HasMeeting Class where
  meeting (Class _name meeting) = meeting

-- in order to make the above code compile.
-- For extra credit, address the other review comment
-- complaining that Meeting really should have an
-- Eq instance. Implement that instance and clean
-- up the code to use it.
instance Eq Meeting where
  (==) (Meeting time1 location1) (Meeting time2 location2) = time1 == time2 && location1 == location2

-- cleaned up version of conflictingMeeting
conflictingMeeting2 :: HasMeeting a => a -> List a -> Maybe a
conflictingMeeting2 newMeeting existingMeetings =
  case existingMeetings of
    Nil -> Nothing
    Cons m ms ->
      if meeting newMeeting == meeting m
        then Just m
        else conflictingMeeting2 newMeeting ms

-- For further extra credit, help out the backend
-- architect and write a function similar to
-- conflictingMeeting, except it works for more
-- things than just stuff with a Meeting value inside.
conflicting :: Eq a => a -> List a -> Maybe a
conflicting a list =
  case list of
    Nil -> Nothing
    Cons x xs ->
      if a == x
        then Just x
        else conflicting a xs

conflictingOn :: Eq b => (a -> b) -> a -> List a -> Maybe a
conflictingOn f x list =
  case list of
    Nil -> Nothing
    Cons a list' ->
      if f a == f x
        then Just x
        else conflictingOn f a list'

conflictingMeeting3 :: HasMeeting a => a -> List a -> Maybe a
conflictingMeeting3 = conflictingOn meeting

class1 = Class "class1" (Meeting 10 "a")

class2 = Class "class2" (Meeting 10 "b")

class3 = Class "class3" (Meeting 15 "a")

class4 = Class "class4" (Meeting 15 "c")

classes = Cons class1 (Cons class2 (Cons class3 (Cons class4 Nil)))

meetingShouldConflict = conflictingMeeting2 class1 classes

meetingShouldNotConflict = conflictingMeeting2 (Class "class" (Meeting 20 "c")) classes

classShouldConflict = conflictingClass2 class1 classes

classShouldNotConflict = conflictingClass (Class "class" (Meeting 20 "c")) classes

otherThingShouldConflict = conflicting 1 (Cons 1 (Cons 2 (Cons 3 Nil)))

otherThingShouldNotConflict = conflicting 1 (Cons 2 (Cons 3 Nil))
