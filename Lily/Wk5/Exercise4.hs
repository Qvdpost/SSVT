{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise4 where

import Data.List
import Helper (exercise)
import System.Random
import Test.QuickCheck
import LTS
--Time Spent:

{-
after :: LTS -> [Label] -> [State]
(states, labels, labeledTransitions, startState) `after` transition = makeTransition
-}

third :: (a, b, c) -> c
third (_,_,s) = s

makeTransition :: State -> [LabeledTransition] -> Label -> [State]
makeTransition s transitions l = map third $ filter (\(s',l',_) -> s == s' && l == l') transitions

-- coffeeImpl1 `after` ["?coin"] -> [2]
-- This works, but it needs to be a trace, not a single label.
after :: LTS -> Label -> [State]
after (_,_,transitions,start) label =
    map third $ filter (\(s,l,_) -> s == start && l == label) transitions

-- after :: LTS -> Trace -> [State]
-- after (_,_,t,s) tr = error "not yet implemented"

exercise4 :: IO ()
exercise4 = do
  putStrLn $ exercise 4 "XXX"
  putStrLn "Example output: "

_main :: IO ()
_main =
  exercise4