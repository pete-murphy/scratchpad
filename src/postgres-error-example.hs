{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch)
import Data.Functor (($>), (<&>))
import Data.Int (Int64)
import Database.PostgreSQL.Simple (FormatError (FormatError), SqlError (..), ToRow, connectPostgreSQL, execute)
import GHC.Generics (Generic)

data User = User String String String
  deriving (Generic)

instance ToRow User

addUser :: IO String
addUser = do
  let x = User "test" "test" "test"
  conn <- connectPostgreSQL "<your username/password/host>"
  (execute conn "INSERT INTO user (username, country, city) values (?, ?, ?)" x $> "User successfully registered")
    `catch` \(_ :: FormatError) -> return "SQL formatting error"
      `catch` \(_ :: SqlError) -> return "SQL error"

addUser' :: IO (Either String Int64)
addUser' = do
  let x = User "test" "test" "test"
  conn <- connectPostgreSQL "<your username/password/host>"
  (execute conn "INSERT INTO user (username, country, city) values (?, ?, ?)" x <&> Right)
    `catch` \(_ :: FormatError) -> return (Left "SQL formatting error")
      `catch` \(_ :: SqlError) -> return (Left "SQL error")
