{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

import Control.Arrow
import Control.Comonad
import Control.Monad.State
import Data.Ord
import Data.Semigroup (Any (..), First (First))
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.Semiring
import Data.Tree
import RIO

-- data Tree a = Node a [Tree a]

tree :: Tree String
tree =
  Node
    "foo"
    [ Node
        "bar"
        [ Node "bar-1" [],
          Node "bar-2" []
        ],
      Node
        "baz"
        [ Node "baz-1" [],
          Node "baz-2" [],
          Node "baz-3" []
        ]
    ]

filterTree :: (a -> Bool) -> Tree a -> Tree a
filterTree predicate =
  extend (foldMap1 (First &&& Any . predicate))
    >>> (\(Node value forest) -> Node value (go forest))
    >>> fmap (\(First x, _) -> x)
  where
    go frst =
      unfoldForest
        (\(Node value frst') -> (value, go frst'))
        (filter (\(Node (_, Any matchesPredicate) _) -> matchesPredicate) frst)

filterTree' :: (a -> Bool) -> Tree a -> Maybe (Tree a)
filterTree' p = foldTree \a maybeSubtrees -> do
  let subtrees = catMaybes maybeSubtrees
  guard (p a || not (null subtrees))
  pure $ Node a subtrees

-- | Input tree:
-- >>> drawTree tree
-- foo
-- |
-- +- bar
-- |  |
-- |  +- bar-1
-- |  |
-- |  `- bar-2
-- |
-- `- baz
--    |
--    +- baz-1
--    |
--    +- baz-2
--    |
--    `- baz-3

-- | Filtered result:
-- >>> drawTree $ filterTree (`elem` ["bar","baz-1","baz-3"]) tree
-- foo
-- |
-- +- bar
-- |
-- `- baz
--    |
--    +- baz-1
--    |
--    `- baz-3

-- withPath :: Tree String -> Tree ([String], String)
-- withPath = foldTree go
--   where
--     go x = \xs -> Node ([], x) (map (fmap (first (x :))) xs)

-- withPath' :: Tree String -> Tree ([String], String)
-- withPath' = flip evalState [] . traverse go
--   where
--     go x = state \xs -> ((xs, x), xs <> [x])

-- >>> drawTree $ fmap show $ withPath tree
-- ([],"foo")

-- |
-- +- (["foo"],"bar")
-- |  |
-- |  +- (["foo","bar"],"1")
-- |  |
-- |  `- (["foo","bar"],"2")
-- |
-- `- (["foo"],"baz")
--    |
--    +- (["foo","baz"],"1")
--    |
--    `- (["foo","baz"],"2")

-- >>> drawTree $ fmap show $ withPath' tree
-- ([],"foo")

-- |
-- +- (["foo"],"bar")
-- |  |
-- |  +- (["foo","bar"],"1")
-- |  |
-- |  `- (["foo","bar","1"],"2")
-- |
-- `- (["foo","bar","1","2"],"baz")
--    |
--    +- (["foo","bar","1","2","baz"],"1")
--    |
--    `- (["foo","bar","1","2","baz","1"],"2")

-- |
-- "([],\"foo\")\n|\n+- ([\"foo\"],\"bar\")\n|  |\n|  +- ([\"foo\",\"bar\"],\"1\")\n|  |\n|  `- ([\"foo\",\"bar\"],\"2\")\n|\n`- ([\"foo\"],\"baz\")\n   |\n   +- ([\"foo\",\"baz\"],\"1\")\n   |\n   `- ([\"foo\",\"baz\"],\"2\")\n"

-- "([],\"foo\")\n|\n+- ([\"foo\"],\"bar\")\n|  |\n|  +- ([\"foo\",\"bar\"],\"1\")\n|  |\n|  `- ([\"foo\",\"bar\",\"1\"],\"2\")\n|\n`- ([\"foo\",\"bar\",\"1\",\"2\"],\"baz\")\n   |\n   +- ([\"foo\",\"bar\",\"1\",\"2\",\"baz\"],\"1\")\n   |\n   `- ([\"foo\",\"bar\",\"1\",\"2\",\"baz\",\"1\"],\"2\")\n"

-- do
--   where
--     f x xs =
--       let xs' = map (fmap (first (x :))) xs
--        in Node ([], x) xs'

-- f x xs =
--   let xs' = fmap (fmap (fmap (x :))) xs
--    in Node (x, []) xs'
-- f x = Node (x, []) . (fmap . fmap . fmap) (x :)

-- >>> import Data.Tree
-- >>> drawTree (fmap show x)
-- "([],\"foo\")\n|\n+- ([\"foo\"],\"bar\")\n|  |\n|  +- ([\"foo\",\"bar\"],\"1\")\n|  |\n|  `- ([\"foo\",\"bar\"],\"2\")\n|\n`- ([\"foo\"],\"baz\")\n   |\n   +- ([\"foo\",\"baz\"],\"1\")\n   |\n   `- ([\"foo\",\"baz\"],\"2\")\n"

--- |
-- treeWithPath :: Tree ([String], String)
-- treeWithPath =
--   Node
--     ([], "foo")
--     [ Node
--         (["foo"], "bar")
--         [ Node (["foo", "bar"], "1") [],
--           Node (["foo", "bar"], "2") []
--         ],
--       Node
--         (["foo"], "baz")
--         [ Node (["foo", "baz"], "1") [],
--           Node (["foo", "baz"], "2") []
--         ]
--     ]

-- transformTree :: Semiring m => Tree a -> (a -> (m, a)) -> Tree (m, a)
-- transformTree (Node x xs) f = undefined
-- -- let

-- I think you ought to be able to do this with Data.Tree.foldTree :: (a -> [b] -> b) -> Tree a -> b with b ~ Tree ([String], String)

-- instance Ord (Down a -> a) where
