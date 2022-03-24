import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (find)

data DynamoDBConfiguration
data DynamoDBItem
data GetItemResponse = GetItemResponse { itemKey :: String }

generateKeys :: [IO String]
generateKeys = undefined

fetchItem :: (MonadCatch m, MonadIO m, MonadUnliftIO m) => DynamoDBConfiguration -> DynamoDBItem -> m GetItemResponse
fetchItem = undefined

dbConfig :: DynamoDBConfiguration
dbConfig = undefined

dbItem :: DynamoDBItem
dbItem = undefined

foo :: (MonadCatch m, MonadIO m, MonadUnliftIO m) => m (Maybe String)
foo = do
  k <- itemKey <$> fetchItem dbConfig dbItem
  keys <- liftIO (sequenceA generateKeys)
  pure (find (== k) keys)