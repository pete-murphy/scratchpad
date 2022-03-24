import Control.Monad.State (StateT (..), evalStateT, get, liftIO)
import Data.Char (isDigit)
import Text.Read (readMaybe)

data Player = X | O

type Board = [Maybe Player]

type GameState = (Board, Player)

main :: IO ()
main = evalStateT gameLoop (replicate 9 Nothing, X)

gameLoop :: StateT GameState IO ()
gameLoop = do
  (board, player) <- get
  liftIO (print (display board))
  nextMove <- liftIO getLine
  case parse nextMove of
    Right validMove -> undefined
    Left _ -> undefined

data Error = InvalidMove

parse :: String -> Either Error Int
parse input = case (readMaybe input :: Maybe Int) of
  Nothing -> Left InvalidMove
  Just input' -> undefined

display :: [Maybe Player] -> String
display = error "not implemented"
-- printB
