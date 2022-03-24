data A = A {idA :: String, valA :: String}

data B = B {idB :: String, valB :: String}

listA :: [A]
listA = [A {idA = "a", valA = "a"}, A {idA = "a", valA = "b"}, A {idA = "a", valA = "c"}]

listB :: [B]
listB = [B {idB = "a", valB = ""}, B {idB = "b", valB = ""}]

data C = C {idC :: String, valC :: String}
  deriving (Show)

f :: [A] -> [B] -> [C]
f listA listB = do
  idB' <- idB <$> listB
  let valA' = valA <$> filter ((== idB') . idA) listA
  pure (C {idC = idB', valC = concat valA'})
-- >>> f listA listB
-- [C {idC = "a", valC = "abc"},C {idC = "b", valC = ""}]
