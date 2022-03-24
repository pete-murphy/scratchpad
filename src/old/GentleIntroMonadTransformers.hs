{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Monad.IO.Class (MonadIO (..))
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data LoginError = InvalidEmail | NoSuchUser | WrongPassword
  deriving (Show)

getDomain :: Text -> Either LoginError Text
getDomain email =
  case T.splitOn "@" email of
    [_name, domain] -> Right domain
    _ -> Left InvalidEmail

printResult :: Either LoginError Text -> IO ()
printResult =
  T.putStrLn
    . either
      (const "ERROR: Invalid domain")
      ("Domain: " <>)

getToken :: EitherIO LoginError Text
getToken = do
  liftIO (T.putStrLn "Enter email address:")
  input <- liftIO T.getLine
  liftEither (getDomain input)

users :: () => Map Text Text
users =
  Map.fromList
    [ ("example.com", "qwerty123"),
      ("localhost", "password")
    ]

userLogin :: EitherIO LoginError Text
userLogin = do
  token <- getToken
  -- userpw <- liftEither $ note NoSuchUser (Map.lookup token users)
  userpw <- maybe (throwE NoSuchUser) pure (Map.lookup token users)
  liftIO (T.putStrLn "Enter password:")
  pw <- liftIO T.getLine
  if pw == userpw then pure pw else throwE WrongPassword

-- token <- getToken
-- userpw <-
--   Map.lookup token users
--     & maybe (liftEither (Left NoSuchUser)) pure
-- pure "asdf"

note :: a -> Maybe b -> Either a b
note def Nothing = Left def
note _ (Just x) = Right x

-- case Map.lookup token users of
--   Just userpw -> do
--     liftIO $ T.putStrLn "Enter password:"
--     pw <- liftIO T.getLine
--     if pw == userpw
--       then pure token
--       else EitherIO (pure (Left WrongPassword))
--   Nothing -> undefined

-- case token of
--   Right domain ->
--     case Map.lookup domain users of
--       Just userpw -> do
--         T.putStrLn "Enter password:"
--         password <- T.getLine
--         if userpw == password
--           then pure token
--           else pure (Left WrongPassword)
--       Nothing -> pure (Left NoSuchUser)
--   left -> pure left

newtype EitherIO e a
  = EitherIO {runEitherIO :: IO (Either e a)}

instance Functor (EitherIO e) where
  fmap :: (a -> b) -> EitherIO e a -> EitherIO e b
  fmap f (EitherIO x) = EitherIO (fmap (fmap f) x)

instance Applicative (EitherIO e) where
  pure = EitherIO . pure . pure

  -- UNLAWFUL INSTANCE ???
  -- liftA2 f (EitherIO x) (EitherIO y) = EitherIO (liftA2 (liftA2 f) x y)
  -- EitherIO f <*> EitherIO x = EitherIO (liftA2 (<*>) f x)
  -- EitherIO f <*> EitherIO x = EitherIO ((liftA2 @IO) (<*>) f x)

  EitherIO f <*> EitherIO x =
    EitherIO do
      f' <- f
      x' <- x
      pure (f' <*> x')

instance Monad (EitherIO e) where
  EitherIO x >>= f =
    EitherIO $
      x >>= \case
        Right r -> runEitherIO (f r)
        Left l -> pure (Left l)

instance MonadIO (EitherIO e) where
  liftIO x = EitherIO (pure <$> x)

liftEither :: Either e a -> EitherIO e a
liftEither x = EitherIO (pure x)

-- instance MonadThrow (EitherIO e) where
--   throwM = liftEither . Left

throwE :: e -> EitherIO e a
throwE = liftEither . Left

catchE :: EitherIO e a -> (e -> EitherIO e a) -> EitherIO e a
catchE (EitherIO throwing) handler =
  EitherIO do
    throwing >>= \case
      Left x -> runEitherIO $ handler x
      Right r -> pure $ Right r
