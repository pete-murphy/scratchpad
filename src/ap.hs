{-# LANGUAGE NoImplicitPrelude #-}

import Prelude hiding ((*>), (<*))

(*>) :: Applicative f => f a -> f b -> f b
fa *> fb = ignoreFirst <$> fa <*> fb
  where ignoreFirst a b = b

(*>) :: Applicative f => f a -> f b -> f b
fa *> fb = ignoreFirst fa fb
  where ignoreFirst a b = b
-- (*>) :: Applicative f => f a -> f b -> f b
