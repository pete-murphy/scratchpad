import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified System.IO as IO
import System.IO (BufferMode (..))

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout NoBuffering
  Concurrent.forkIO (Monad.replicateM_ 10000 (IO.putChar '-'))
  Monad.replicateM_ 10000 (IO.putChar '|')
