{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

import Data.Function (on)

data Foo = Foo deriving (Show)

instance Show a => Eq a where
  (==) = (==) `on` show
