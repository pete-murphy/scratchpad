import Control.Comonad
import Control.Lens
import Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as PE
import Data.Maybe

instance Comonad PointedList where
  extend = PE.contextMap
  extract = PE._focus

water :: [Int] -> Int
water = view _2 . wfix . fmap go . fromMaybe (PE.singleton 0) . PE.fromList
  where
    go height context = (lMax, total, rMax)
      where
        get f = maybe (height, 0, height) PE._focus $ f context
        (prevLMax, _, _) = get PE.previous
        (_, prevTotal, prevRMax) = get PE.next
        lMax = max height prevLMax
        rMax = max height prevRMax
        total = prevTotal + min lMax rMax - height
-- water' :: [Int] -> Int
-- water' = view _2 . kfix . fmap go . fromMaybe (PE.singleton 0) . PE.fromList
--   where
--     go height context = (lMax, total, rMax)
--       where
--         get f = maybe (height, 0, height) PE._focus $ f context
--         (prevLMax, _, _) = get PE.previous
--         (_, prevTotal, prevRMax) = get PE.next
--         lMax = max height prevLMax
--         rMax = max height prevRMax
--         total = prevTotal + min lMax rMax - height
