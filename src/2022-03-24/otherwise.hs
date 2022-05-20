-- passes the exhaustivity checker
foo :: Bool -> ()
foo b
  | b = ()
  | otherwise = ()

-- passes the exhaustivity checker
foo' :: Bool -> ()
foo' b
  | b = ()
  | True = ()

-- fails the exhaustivity checker:
--   Pattern match(es) are non-exhaustive
--   In an equation for 'foo': Patterns not matched: False
foo'' :: Bool -> ()
foo'' b
  | b = ()
  | myOtherwise = ()
  where
    myOtherwise = otherwise
