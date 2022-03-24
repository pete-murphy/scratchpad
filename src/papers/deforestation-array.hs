{-# LANGUAGE RankNTypes #-}

import GHC.Arr (Array)

type AT a b = Array a b -> Array a b

createAT :: (a, a) -> AT a b -> Array a b
createAT = undefined

writeAT :: a -> b -> AT a b
writeAT = undefined

seqAT :: AT a b -> AT a b -> AT a b
seqAT = undefined

doneAT :: AT a b
doneAT = undefined

isLeapYear :: Integer -> Bool
isLeapYear year = divisibleBy 400 || (divisibleBy 4 && not $ divisibleBy 100)
  where 
   divisibleBy x = mod y x == 0