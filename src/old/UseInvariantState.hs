{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module UseInvariantState where 

data UseState a
  = UseState
    { state :: a
    , setState :: (a -> a) -> ()
    }

class Invariant f where
  imap :: (b -> a) -> (a -> b) -> f a -> f b

instance Invariant UseState where
  imap :: (b -> a) -> (a -> b) -> UseState a -> UseState b
  imap f g UseState{..} = UseState (g state) (setState . \h -> f . h . g)
