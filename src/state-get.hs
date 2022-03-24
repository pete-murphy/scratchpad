import qualified Control.Monad.State as State
import Control.Monad.State (StateT)

gameLoop :: StateT Int IO ()
gameLoop = do
  userInput <- State.lift getLine
  pure ()
