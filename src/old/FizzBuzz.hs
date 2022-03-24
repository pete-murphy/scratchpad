module FizzBuzz where

type FizzRule = Integer -> Maybe String

rule :: Integer -> String -> FizzRule
rule n m i =
  case i `mod` n of
    0 -> Just m
    _ -> Nothing

fizz, buzz :: FizzRule
fizz = rule 3 "Fizz"

buzz = rule 5 "Buzz"
