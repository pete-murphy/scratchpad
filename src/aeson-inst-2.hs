{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (withObject)
import Data.Either (either)
import Data.Function ()
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
-- import Flow
import Network.HTTP.Req

data Todo
  = Todo
      { userId :: Text,
        id :: Text,
        title :: Text,
        completed :: Bool
      }
  deriving (Show)

instance FromJSON Todo where
  parseJSON = withObject "Todo" \v ->
    Todo
      <$> v .: "userId"
      <*> v .: "id"
      <*> v .: "title"
      <*> v .: "completed"

main :: IO ()
main = runReq defaultHttpConfig $ do
  r <- req GET (https "jsonplaceholder.typicode.com" /: "todos" /: "1") NoReqBody jsonResponse mempty
  liftIO $ print (responseBody r :: Todo)
