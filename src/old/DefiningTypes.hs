module DefiningTypes where

type LastName = String

type FirstName = String

bill :: FirstName
bill = "Bill"

murray :: LastName
murray = "Murray"

data Person =
  Person
    { firstName :: FirstName
    , lastName  :: LastName
    }

bm = Person bill murray

newtype FirstName' =
  FirstName' String

newtype LastName' =
  LastName' String
