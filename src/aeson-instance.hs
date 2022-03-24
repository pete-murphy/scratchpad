{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:), FromJSON (..), withObject)
import Data.Text (Text)
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
  parseJSON = withObject "Todo" $ \v ->
    Todo
      <$> v .: "userId"
      <*> v .: "id"
      <*> v .: "title"
      <*> v .: "completed"

main :: IO ()
main = runReq defaultHttpConfig $ do
  r <- req GET (https "jsonplaceholder.typicode.com" /: "todos" /: "1") NoReqBody jsonResponse mempty
  liftIO $ print (responseBody r :: Todo)
