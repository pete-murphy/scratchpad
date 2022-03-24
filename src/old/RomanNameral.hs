module RomanNameral where

import           Control.Monad (forever)
import           Data.Char     (toUpper)
import           Data.Maybe    (mapMaybe)

romanNumerals :: [(Char, Int)]
romanNumerals =
  [ ('I', 1)
  , ('V', 5)
  , ('X', 10)
  , ('L', 50)
  , ('C', 100)
  , ('D', 500)
  , ('M', 1000)
  , ('Y', 5000)
  ]

parseNameral :: String -> Int
parseNameral = sum . mapMaybe (flip lookup romanNumerals . toUpper)

main :: IO ()
main = do
  putStrLn "Enter a name:"
  name <- getLine
  print (parseNameral name) >> main
