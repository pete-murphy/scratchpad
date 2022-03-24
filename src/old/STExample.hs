module STExample where

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef

two :: Int
two =
  runST $ 
   do
    n <- newSTRef 1
    writeSTRef n 2
    readSTRef n

two' :: Int
two' = runST $ newSTRef 1 >>= \n -> writeSTRef n 2 >> readSTRef n

main = print two >> print two'
