module Password where

import Control.Monad (forM_)
import Data.Char (toUpper)

passwords :: [String]
passwords = sequenceA (map (\c -> [c, toUpper c]) "password")

main :: IO ()
main = forM_ passwords putStrLn
