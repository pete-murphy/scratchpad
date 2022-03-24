module Part1 where

import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P (decimal)

type Parser = P.Parsec () String

-- 4-7 z: zzzfzlzzz
data Row = Row Int Int Char String deriving (Show)

-- type State s a = s -> (a, s)
type Parser' input output = input -> Maybe (output, input)

rowParser :: Parser Row
rowParser = do
  mn <- P.decimal
  P.char '-'
  mx <- P.decimal
  P.space
  c <- P.asciiChar
  P.char ':'
  P.space
  pass <- P.manyTill P.asciiChar P.newline
  pure (Row mn mx c pass)

isRowValid :: Row -> Bool
isRowValid (Row mn mx c pass) =
  let numCs = countPred (== c) pass
   in numCs >= mn && numCs <= mx

countPred :: (a -> Bool) -> [a] -> Int
countPred f = length . filter f

rowsParser :: Parser [Row]
rowsParser = P.many rowParser

countValidRows :: [Row] -> Int
countValidRows rows = countPred isRowValid rows
