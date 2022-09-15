data DeptRef

data User f
  = User
      { userName :: f String,
        userAge :: f Int,
        userDept :: f DeptRef
      }

type Identity a = a

x = userName (undefined :: User (Identity _))
-- >>> :t
-- The type synonym ‘Identity’ should have 1 argument, but has been given none

-- >>> userName (undefined :: User Gen) :: Gen String

-- type family F where
--   F Identity a = a
--   F f a = f a
