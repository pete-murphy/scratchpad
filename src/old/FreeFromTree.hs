module FreeFromTree where

import           Data.Functor.Compose (Compose)
import           Data.Map             (Map)
import qualified Data.Map             as M

data AST f a
  = Leaf a
  | Branch (f (AST f a))

-- data Tree a
--   = Leaf a
--   | Branch (Pair (Tree a))
type Tree = AST Pair

data Pair a =
  Pair a a

-- data Math a
--   = Lit a
--   | Op (Operator (Math a))
type Math = AST Operator

data Operator a
  = Add a a
  | Sub a a
  | Mul a a
  | Neg a

treeA :: Tree Int
treeA =
  Branch
    (Pair
       (Branch
          (Pair
             (Branch (Pair (Leaf 1) (Leaf 2)))
             (Branch (Pair (Leaf 3) (Leaf 4)))))
       (Leaf 5))

type MyLang = AST Operation Term

type Identifier = String

data Operation a
  = LetIn Identifier a a
  | Function Identifier a
  | Apply a a

data Term
  = Var Identifier
  | Num Int

-- let addOne = (\a -> a + 1)
--  in addOne 42
example :: MyLang
example =
  Branch
    (LetIn
       "addOne"
       (Branch
          (Function
             "a"
             (Branch
                (Apply
                   (Branch (Apply (Leaf (Var "+")) (Leaf (Var "a"))))
                   (Leaf (Num 1))))))
       (Branch (Apply (Leaf (Var "addOne")) (Leaf (Num 42)))))

type List = []

type PairMaybe = Compose Pair Maybe

data Rose' a =
  BranchR' a (List (Rose' a))

data BST' a =
  BranchB' a (PairMaybe (BST' a))

data BST'' a =
  BranchB'' a (Pair (Maybe (BST'' a)))

exampleRose :: Rose' Int
exampleRose = BranchR' 20 []

data Annotated f a =
  Ann a (f (Annotated f a))

type Rose a = Annotated List a

type BST a = Annotated PairMaybe a

type StrMap = Map String

type Trie a = Annotated StrMap a

toLookupTrie :: [(String, a)] -> Trie (Maybe a)
toLookupTrie = undefined

foo :: Num a => [(String, a)]
foo = [("foo", 1), ("bar", 2), ("baz", 3), ("qux", 4), ("quxx", 5)]

foo' :: Num a => Trie (Maybe a)
foo' =
  Ann Nothing $
  M.fromList
    [ ("foo", Ann (Just 1) M.empty)
    , ( "b"
      , Ann Nothing $
        M.fromList [("ar", Ann (Just 2) M.empty), ("az", Ann (Just 3) M.empty)])
    , ("qux", Ann (Just 4) $ M.fromList [("x", Ann (Just 5) M.empty)])
    ]
