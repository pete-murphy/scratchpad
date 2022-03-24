class TwoButton a where
  press :: a -> (a, a)

pressLeft, pressRight :: TwoButton a => a -> a
pressLeft = fst . press
pressRight = snd . press

class Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)

-- data CofreeTwoButton = Memo CofreeTwoButton CofreeTwoButton

-- memoiseTwoButton :: TwoButton m => m -> CofreeTwoButton
-- memoiseTwoButton m = Memo (memoiseTwoButton (pressLeft m)) (memoiseTwoButton (pressRight m))

data CofreeTwoButton a = Memo a (CofreeTwoButton a) (CofreeTwoButton a)

memoiseTwoButton :: TwoButton m => (m -> a) -> m -> CofreeTwoButton a
memoiseTwoButton f m = Memo (f m) (memoiseTwoButton f (pressLeft m)) (memoiseTwoButton f (pressRight m))
