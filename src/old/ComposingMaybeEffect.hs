{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad (join)
import Data.Foldable (for_, traverse_)

data EventTarget

data HTMLInputElement

data FileList

data File

name = undefined

items = undefined

files = undefined

fromEventTarget = undefined

setFileList = undefined

fromEventTarget :: EventTarget -> Maybe HTMLInputElement

files :: HTMLInputElement -> IO (Maybe FileList)

items :: FileList -> [File]

name :: File -> String

setFileList :: [String] -> IO ()

handleChange :: EventTarget -> IO ()
handleChange t = do
  maybeFiles <- case fromEventTarget t of
    Nothing -> pure Nothing
    Just inputElement' -> files inputElement'
  for_ maybeFiles (setFileList . map name . items)

handleChange' :: EventTarget -> IO ()
handleChange' t =
  join <$> traverse files (fromEventTarget t)
    >>= traverse_
      (setFileList . map name . items)

f >>> g = g . f

g <<< f = g . f

handleChange'' :: EventTarget -> IO ()
handleChange'' = fromEventTarget >>> traverse_ \fileInput -> do
  maybeFiles <- files fileInput
  for_ maybeFiles (setFileList . map name . items)

-- handleChange t =
--   join <$> traverse HTMLInputElement.files (HTMLInputElement.fromEventTarget t)
--     >>= traverse_ (FileList.items >>> map File.name >>> setFileList)
