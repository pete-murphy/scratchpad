module MonadDoVsApp where

maybeA = Just 1

maybeB = Just 2

maybeC = Nothing

foo = do
  a <- maybeA
  b <- maybeB
  c <- maybeC
  Just (a + b)
