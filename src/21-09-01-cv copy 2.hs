{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Arrow

type List a = [a]
type Tuple a b = (a,b)

(%) :: Int -> Int -> Int
(%) = undefined
infixl 7 %

newtype RatioInt = RatioInt Int
 deriving newtype Num

foldTuples :: List (Tuple Int Int) -> RatioInt
foldTuples = sum . fmap (RatioInt <<< uncurry (%))