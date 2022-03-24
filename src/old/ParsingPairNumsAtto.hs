{-# LANGUAGE OverloadedStrings #-}

module ParsingPairNumsAtto where

import Control.Monad.Combinators (skipManyTill)
import Data.Attoparsec.Text (Parser, anyChar, char, decimal)

intPair :: Parser (Int, Int)
intPair = skipManyTill anyChar $ do
  n <- char '(' *> decimal
  m <- char '/' *> decimal <* char ')'
  pure (n, m)
