import Control.Arrow ((&&&))
import qualified Control.Monad as Monad
import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Data.Function ((&))

data Node = Node {children :: [Node], metadata :: [Int]}

allMetadata :: Node -> [Int]
allMetadata node = metadata node ++ concatMap allMetadata (children node)

getInt :: State [Int] Int
getInt = do
  -- first : rest <- State.get
  -- 👆 The above is valid if using GHC < 8.8.1, but recent versions will
  -- complaing about missing 'MonadFail' instance for 'Identity' ('State' is
  -- defined as 'StateT Identity', and the pattern match might fail. If you want
  -- to dig further into the weeds, see:
  -- https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad-Fail.html#t:MonadFail)
  (first, rest) <- State.gets (head &&& tail)
  State.put rest
  pure first

getNode :: State [Int] Node
getNode = do
  n <- getInt
  m <- getInt
  Node <$> Monad.replicateM n getNode <*> Monad.replicateM m getInt

main :: IO ()
main = do
  input <- readFile "input"
  input
    & words
    & map read
    & State.evalState getNode
    & allMetadata
    & sum
    & print
