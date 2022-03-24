{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Control.Monad.State
import Prelude

{-
string compressing
"aaabbddd" -> "a3b2d3"
"hello" -> "h1e1l2o1"
-}
{-
"hello"
-> (('h', 1), "ello")
-> (('e', 1), "llo")
-> (('l', 1), "lo")
-> (('l', 2), "0")
-> (('0', 1), "")
-}

p1 :: String -> Maybe (Char, Int, String) -> String
p1 [] Nothing = []
p1 [] (Just (c, i, s)) = s <> (c : (show i))
p1 (c : cs) Nothing = p1 cs (Just (c, 1, ""))
p1 (c : cs) (Just (c1, i, s))
  | c == c1 = p1 cs (Just (c, i + 1, s))
  | otherwise = p1 cs (Just (c, 1, (s <> (c1 : (show i)))))

-- type State s a = s -> (a, s)

-- execute :: s -> State s a -> s
-- execute initial f = snd (f initial)

-- traverseWithState :: (a -> State s b) -> [a] -> State s [b]
-- traverseWithState _ [] = \s -> ([], s)
-- traverseWithState f (x : xs) = \s ->
--   let z = f x
--       (b', s) = z s
--    in (, s)

-- f :: Char -> [(Char, Int)] -> ((), [(Char, Int)])
-- f c [] = ((), [(c, 1)])
-- f c acc@((c', n) : xs) = if c == c' then ((), (c, n + 1) : xs) else ((), (c, 1) : acc)

-- foo = traverse (\c -> State (\acc@((c',n):xs) -> if c == c' then ((),(c,n+1):xs) else ((),(c,1):acc)))

-- x :: [Char] -> [(Char, Int)]
-- x = execute [] . traverseWithState f

-- >>> x "foo"
-- ProgressCancelledException

-- z = traverse (\(c::Char) -> )

p2 :: String -> State [(Char, Int)] [x]
p2 = traverse $ \c -> state $ \case
  [] -> _what
  ((c', n) : xs) -> _hmm
-- p2 :: String -> State [(Char, Int)] [_]
-- p2 = traverse $ \c -> state $ \case
--   [] -> (_, [(c,1)])
--   ((c', n) : xs) -> _mm

-- >>> execState (p2 "hello") []
