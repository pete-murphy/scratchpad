{-# LANGUAGE LambdaCase #-}

import Data.Foldable (for_, traverse_)
import Data.Set (Set)
import Data.Tree (Tree (..))
import qualified Data.Tree as Tree

findTopSubTreesContaining :: Eq a => Set a -> Tree a -> [Tree a]
findTopSubTreesContaining labels tree =
  if rootLabel tree `elem` labels
    then [tree]
    else findTopSubTreesContaining labels =<< subForest tree

filterTopSubTrees :: (a -> Bool) -> Tree a -> [Tree a]
filterTopSubTrees pred tree =
  if pred (rootLabel tree)
    then [tree]
    else filterTopSubTrees pred =<< subForest tree

example :: Tree String
example =
  Node
    "."
    [ Node
        "a"
        [ Node "aa" [],
          Node "ab" []
        ],
      Node
        "b"
        [ Node "ba" []
        ],
      Node
        "c"
        [ Node "ca" [],
          Node
            "cb"
            [ Node "cba" [],
              Node "cbb" []
            ]
        ]
    ]

drawTree' :: Tree String -> String
drawTree' = map replaceChar . Tree.drawTree
  where
    replaceChar = \case
      '`' -> '└'
      '|' -> '│'
      '+' -> '├'
      '-' -> '─'
      c -> c

main :: IO ()
main = do
  let nodes = ["b", "cb"]
      filtered = filterTopSubTrees (`elem` nodes) example
  -- traverse_ (putStrLn . drawTree') filtered
  putStrLn (drawTree' (Node "." filtered))
-- putStrLn (drawTree' example)
