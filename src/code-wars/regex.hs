{-# LANGUAGE BlockArguments #-}

module RegExpParser
  ( RegExp (..),
    parseRegExp,
  )
where

import Control.Applicative (liftA2)

data RegExp
  = Normal Char --  A character that is not in "()*|."
  | Any --  Any character
  | ZeroOrMore RegExp --  Zero or more occurances of the same regexp
  | Or RegExp RegExp --  A choice between 2 regexps
  | Str [RegExp] --  A sequence of regexps.
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> (String, Maybe a)}

instance Functor Parser where
  fmap f (Parser g) = Parser \s -> (fmap . fmap) f (g s)

instance Applicative Parser where
  pure x = Parser \s -> (s, Just x)
  liftA2 f (Parser g) (Parser h) = Parser \s ->
    let (s', x) = g s
        (s'', y) = h s'
     in (s'', liftA2 f x y)

parseRegExp :: String -> Maybe RegExp
parseRegExp = undefined
