
newtype Reader r a = Reader { runReader :: r -> a }

instance Monad (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader \_ -> a
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  Reader ra >>= arb = Reader \r ->
    let a = ra r
        Reader rb = arb a
     in (rb r :: b)

