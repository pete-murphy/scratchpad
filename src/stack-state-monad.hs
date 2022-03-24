{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.State (MonadState (get), evalState)

f :: (MonadState Int m, MonadState Char m) => m String
f = do
  c <- get @Char
  i <- get @Int
  pure $ c : show i

t = (evalState @Int) f 0
