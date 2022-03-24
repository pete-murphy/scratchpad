import Control.Monad (void)
import Debug.Trace (trace)
import System.IO

data Foo
  = Foo
      { foo :: String,
        bar :: String
      }
  deriving (Show)

mkFoos :: [Foo]
mkFoos = do
  trace "1" (pure ())
  trace "2" $
    pure
      Foo
        { foo = trace "3" "foo",
          bar = trace "4" "bar"
        }

main = do
  hSetBuffering stderr NoBuffering
  hSetBuffering stdout NoBuffering
  pure $ mkFoos `seq` ()
