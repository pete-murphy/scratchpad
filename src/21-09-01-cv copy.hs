{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- module Main where

import           RIO
import qualified RIO.Map as M

data Question = Question
    { qId    :: Text
    , qText  :: Text
    , qScore :: Int
    }
    deriving (Show, Eq)

data Report = Report
    { rId       :: Text
    , rName     :: Text
    , questions :: Map Text Int
    }
    deriving (Show, Eq)

callReports' :: [Report]
callReports' = [ Report "id1" "name1" mempty
               , Report "id2" "name2" mempty
               , Report "id3" "name3" mempty
               ]

questions' :: [Question]
questions' = [ Question "id1" "Q1" 10
             , Question "id1" "Q2" 20
             , Question "id2" "Q3" 30
             , Question "id2" "Q4" 40
             , Question "id3" "Q5" 50
             , Question "id3" "Q5" 60
             ]

-- f :: [A] -> [B] -> [C]
-- f listA listB = do
--   idB' <- idB <$> listB
--   let valA' = valA <$> filter ((== idB') . idA) listA
--   pure (C {idC = idB', valC = concat valA'})

myfunc :: [Question] -> [Report] -> [Report]
myfunc questions oldCallsReports =
  map (\Report rId' rName' m -> Report (qText &&& qScore) <$> filter ((== rId) . qId) questions)) oldCallsReports
  -- let q = 
  -- pure (Report rId' rName' (M.fromList q))

poop :: [Report]
poop = myfunc questions' callReports'

main :: IO ()
main = print poop

-- >>> poop
-- [Report {rId = "id1", rName = "name1", questions = fromList [("Q1",10),("Q2",20)]},Report {rId = "id2", rName = "name2", questions = fromList [("Q3",30),("Q4",40)]},Report {rId = "id3", rName = "name3", questions = fromList [("Q5",60)]}]

--
-- Returns:
-- [ Report {rId = "id1", rName = "name1", questions = fromList [("Q1",10)]}
-- , Report {rId = "id1", rName = "name1", questions = fromList [("Q2",10)]}
-- , Report {rId = "id1", rName = "name1", questions = fromList [("Q3",10)]}
-- , Report {rId = "id1", rName = "name1", questions = fromList [("Q4",10)]}
-- , Report {rId = "id1", rName = "name1", questions = fromList [("Q5",10)]}
-- , Report {rId = "id1", rName = "name1", questions = fromList [("Q5",10)]}
-- ]
--
-- But expected:
-- [ Report {rId = "id1", rName = "name1", questions = fromList [("Q1",10), ("Q2",20)]}
-- , Report {rId = "id1", rName = "name1", questions = fromList [("Q1",30), ("Q2",40)]}
-- , Report {rId = "id1", rName = "name1", questions = fromList [("Q1",50), ("Q2",60)]}
-- ]
