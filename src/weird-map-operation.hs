{-# LANGUAGE BlockArguments #-}

import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.State (State)
import Data.Map (Map)
import qualified Data.Map.Internal as Map

data Span
  = Span
      { value :: Int,
        start :: Char,
        end :: Char
      }

input1 :: [Span]
input1 =
  [Span 100 'a' 'd']

output1 :: Map Char [Int]
output1 =
  Map.fromList
    [ ('a', [100]),
      ('d', [100])
    ]

input2 :: [Span]
input2 =
  [ Span 100 'a' 'd',
    Span 23 'b' 'e'
  ]

output2 :: Map Char [Int]
output2 =
  Map.fromList
    [ ('a', [100]),
      ('d', [100])
    ]

-- [ ('a', [100, 0]),
--   ('b', [100, 123]),
--   ('d', [100, 123]),
--   ('e', [0, 23])
-- ]

input3 :: [Span]
input3 =
  [ Span 100 'a' 'd',
    Span 200 'b' 'e',
    Span 50 'd' 'f'
  ]

output3 :: Map Char [Int]
output3 =
  Map.fromList
    [ ('a', [100, 0, 0]),
      ('b', [100, 200, 0]),
      ('d', [100, 200, 50]),
      ('e', [0, 200, 50]),
      ('f', [0, 0, 50])
    ]
-- aggregateSpans :: [Span] -> Map Char [Int]
-- aggregateSpans s = State.execState (traverse f s) Map.empty
--   where
--     f :: Span -> State (Map Char [Int]) Int
--     f = undefined
--  State.modify

--  Map.merge
