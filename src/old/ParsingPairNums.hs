{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ParsingPairNums where

import Control.Monad.Error.Class
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec as Megaparsec
import Text.Megaparsec (ErrorItem (..), Parsec (..))
import Text.Megaparsec.Char as Megaparsec.Char
import Text.Read (readMaybe)

type Parser = Parsec Void Text

int :: Parser Int
int = do
  n <- Megaparsec.many (Megaparsec.Char.digitChar <|> Megaparsec.Char.char ',')
  case readMaybe n of
    Just n' -> pure n'
    Nothing -> Megaparsec.unexpected Megaparsec.EndOfInput

intPair :: Parser (Int, Int)
intPair = Megaparsec.skipManyTill Megaparsec.anySingle $ do
  _ <- Megaparsec.Char.char '('
  n <- int
  _ <- Megaparsec.Char.char '/'
  m <- int
  _ <- Megaparsec.Char.char ')'
  pure (n, m)

foo :: Int -> Int
foo = id@Int
