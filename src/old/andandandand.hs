data Env = Env { a :: Int, b :: Int, c :: Int }

tupleFromEnv :: Env -> (Int, Int, Int)
tupleFromEnv = (,,) <$> a <*> b <*> c