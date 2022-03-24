{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (zipWithM)
import Data.Functor ((<&>))
import Prelude hiding (traverse)
import qualified Prelude as P

data Tree a
  = Tree
      { value :: a,
        forest :: [Tree a]
      }

traverse :: forall m a b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traverse = go
  where
    go f ta =
      do
        x' <- f (value ta)
        xs' <- go f `P.traverse` forest ta
        pure (Tree x' xs')

traverseWithIndex :: forall m a b. Applicative m => ([Int] -> a -> m b) -> Tree a -> m (Tree b)
traverseWithIndex f = go f []
  where
    -- go ::  [Int] -> Tree a -> m (Tree b)
    go f ns ta = do
      value' <- f ns (value ta)
      -- xs <- zipWithM go ([0..] <&> (\x -> ns ++ [x])) (forest ta)
      forest' <- sequenceA (zipWith (go f) ([0 ..] <&> (\x -> ns ++ [x])) (forest ta))
      pure (Tree value' forest')

exampleTree :: Tree String
exampleTree =
  Tree
    "foo"
    [ Tree
        "bar"
        [ Tree "bar-1" [],
          Tree "bar-2" []
        ],
      Tree
        "baz"
        [ Tree "baz-1" [],
          Tree "baz-2" [],
          Tree
            "baz-3"
            [ Tree "baz-3-1" []
            ]
        ]
    ]
