{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module ValidateForm where

import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable

type Error = String

toRight :: Either a b -> (forall x. Either x (Maybe a))
toRight = \case
  Left x -> Right (Just x)
  Right _ -> Right Nothing

swapEither :: Either a b -> Either b a
swapEither = \case
  Left x -> Right x
  Right x -> Left x

validateForm ::
  Ord k =>
  Map k a ->
  Map k (a -> Either Error b) ->
  Either (Map k (Maybe Error)) (Map k b)
validateForm m mf =
  Map.intersectionWith ($) mf m
    & \validated -> sequenceA validated
      & \case
        Left _ ->
          validated
            & fmap toRight
            & sequenceA
            & swapEither
        Right x -> Right x

isNonEmpty :: String -> Either Error String
isNonEmpty str = if null str then Left "Empty String" else Right str

validForm = Map.fromList [("name", "Joe"), ("occupation", "Plumber")]

partlyValidForm = Map.fromList [("name", "Joe"), ("occupation", "")]

invalidForm = Map.fromList [("name", ""), ("occupation", "")]

validators = Map.fromList [("name", isNonEmpty), ("occupation", isNonEmpty)]

main :: IO ()
main = do
  print (validateForm validForm validators)
  print (validateForm partlyValidForm validators)
  print (validateForm invalidForm validators)
