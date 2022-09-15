import Data.Char
import GHC.Float
import Control.Monad.State

-- | Get the function for the operation corresponding to an
-- operator character
chrToOp
  :: (Num a, Fractional a)
  => Char           -- ^ Character denoting an operation
  -> (a -> a -> a)  -- ^ Function corresponding to the operation
chrToOp ch =
  case ch of
    '+' -> (+)
    '-' -> (-)
    '*' -> (*)
    '/' -> (/)
    _  -> (+)   -- Ignoring the error


-- | Check if a character corresponds to an operator
isOp
  :: Char  -- ^ Input character
  -> Bool
isOp ch
  | elem ch "+-*/" = True
  | otherwise      = False


-- | Change the stack state using an input character
foo
  :: Char     -- ^ Input character
  -> [Float]  -- ^ Current stack value
  -> [Float]  -- ^ Next stack value
foo ch st
  -- | isDigit ch = [read ch :: Float] ++ st
  -- | isDigit ch = [digitToInt (int2Float ch)] ++ st
  | isDigit ch = [int2Float $ digitToInt ch] ++ st
  | isOp    ch = newstate
  | otherwise  = st
 where
  -- add new val after 'popping' 2 elements from stack
  newstate = [newval] ++ (tail (tail st))
  operation = chrToOp ch
  ab = take 2 st
  newval = operation (ab!!0) (ab!!1)
--
type StateType = [Float]
type ValType = Float

trafn
  :: String -- ^ Input
  -> State StateType ValType

trafn [] = do
  curst <- get -- If all went well, stack length should be 1
  return (head curst)

trafn (x:xs) = do
  curst <- get
  newst <- foo x curst
  put newst
  trafn xs