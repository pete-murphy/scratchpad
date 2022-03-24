module Points where

import           System.IO (isEOF)

data Point =
  Point String [Float]
  deriving (Eq, Ord, Show)

getFloat = read

main :: IO ()
main = myLoop []

myLoop :: [Point] -> IO ()
myLoop p = do
  done <- isEOF
  if done
    then print p
    else do
      inp <- getLine
      let (label:coord) = words inp
      myLoop $ p ++ [Point label (map getFloat coord)]
