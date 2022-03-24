import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Identity (Identity)
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

type Parser = P.Parsec Void String

data Stuff

parseStuff :: Parser Stuff
parseStuff = undefined

parseStuffArray :: Parser [Stuff]
parseStuffArray =
  do
    sat (== '\n')
    r <- parseStuff
    rs <- parseStuffArray
    return (r : rs)
    <|> do
      r <- parseStuff
      rs <- parseStuffArray
      return (r : rs)

sat = P.satisfy

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither pa pb = (Left <$> pa) <|> (Right <$> pb)

parseTuple :: Parser a -> Parser b -> Parser (a, b)
parseTuple pa pb = (,) <$> pa <*> pb

-- type
