import Data.Maybe (fromJust)

countPositivesSumNegatives :: Maybe [Integer] -> [Integer]
countPositivesSumNegatives l =
  let arr = fromJust l
      lessThenZero = toInteger $ sum [n | n <- arr, n < 0]
      moreThenzero = toInteger $ length [n | n <- arr, n > 0]
   in [moreThenzero, lessThenZero]
