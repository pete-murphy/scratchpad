import Data.Char (isAlphaNum)
import Data.Monoid (All (All), getAll)

atLeast :: Int -> String -> Bool
atLeast l xs = length xs > l

allAlphaNum :: String -> Bool
allAlphaNum = all isAlphaNum

-- validPassword :: String -> Bool
-- validPassword pw = all ($ pw) [atLeast 6, allAlphaNum]

validPassword :: String -> Bool
validPassword = getAll <$> foldMap (All .) [atLeast 6, allAlphaNum]
