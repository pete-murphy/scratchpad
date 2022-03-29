{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

import Data.Function

data Eval a where
  Now :: a -> Eval a
  Chain :: (a -> Eval b) -> Eval a -> Eval b

now9 :: Eval String
now9 = Now 9 & Chain (Now . show)
-- Chain :: (Eval b, b -> Eval a) -> Eval a

-- data Eval a = Now a | Chain (a, a -> Eval b)
--   -- :t Now   :: a -> Eval a
--   -- :t Chain :: (a, a -> Eval b) -> Eval a
