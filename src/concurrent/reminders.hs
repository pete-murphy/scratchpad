{-# LANGUAGE BlockArguments #-}

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Text.Printf as Printf

main :: IO ()
main = Monad.forever do
  s <- getLine
  Concurrent.forkIO (setReminder s)

setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  Printf.printf "Ok, I'll remind you in %d seconds\n" t
  Concurrent.threadDelay (10 ^ 6 * t)
  Printf.printf "%d seconds is up!\n" t
