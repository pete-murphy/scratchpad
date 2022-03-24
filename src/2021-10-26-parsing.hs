{-# LANGUAGE LambdaCase #-}

import Control.Applicative (Alternative ((<|>)), optional)
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P (decimal)

type Parser = P.Parsec () String

-- expr   ::= term + expr | term
-- term   ::= factor * term | factor
-- factor ::= (expr) | a
data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Lit Int

instance Show Expr where
  show = \case
    Add x y -> "Add " <> show x <> " " <> show y
    Mul x y -> "Mul " <> show x <> " " <> show y
    Lit x -> show x

expr :: Parser Expr
expr =
  P.try (do
    x <- term
    _ <- P.char '+'
    Add x <$> expr)
    <|> term

term :: Parser Expr
term =
    P.try (do x <- factor
              _ <- P.char '*'
              Mul x <$> term)
    <|> factor

term' :: Parser Expr
term' =
  do
    x <- factor
    _ <- P.char '*'
    y <- term
    optional P.eof
    pure (Mul x y)

factor :: Parser Expr
factor =
  P.try (do
    _ <- P.char '('
    y <- expr
    _ <- P.char ')'
    expr)
  <|> lit

lit :: Parser Expr
lit = Lit <$> P.decimal

main :: IO ()
main = do
  print (P.parseMaybe term "2*3")
  print (P.parseMaybe term' "2*3")
  print (P.parseMaybe expr "2*3")
