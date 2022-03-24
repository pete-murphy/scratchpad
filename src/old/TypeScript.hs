{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (SomeException, catch)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (null)

-- THE BILLION DOLLAR MISTAKE.
null :: any
null = undefined

-- BEST PRACTICES.
divide :: (Eq x, Fractional x) => x -> x -> x
divide this that = if that == 0 then null else this / that

isNull :: any -> Bool
isNull x = unsafePerformIO do
  catch (x `seq` pure False) \(e :: SomeException) -> pure True

(??) :: any -> any -> any
value ?? fallback = if isNull value then fallback else value


(?.) :: any -> (any -> any) -> any
value ?. fn = if isNull value then null else fn value

-- Increment a number up to 2, then return null
inc () n = if n < 2 then n + 1 else null

main :: IO ()
main = do
  print (1?.inc() ?? 99) -- prints "3"
  print (1?.inc()?.inc() ?? 99) -- prints "99"
