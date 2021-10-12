{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise2 where

import Data.List (intersect, nub, sort, union, (\\))
import Exercise1 (validateLTS)
import Helper (exercise)
import LTS
import Test.QuickCheck (Arbitrary, Gen, Positive, Property, arbitrary, elements, forAll, generate, listOf, listOf1, quickCheck, sample, shuffle, suchThat, vectorOf, (==>))

-- Time Spent:

-- ltsGen :: Gen IOLTS
-- ltsGen =

-- ([State], [Label], [Label], [LabeledTransition], State)

-- arbitraryLabeledTransition :: Gen LabeledTransition
-- arbitraryLabeledTransition = do
--   i <- arbitrary :: Gen Integer
--   j <- arbitrary :: Gen Integer
--   randomString <- listOf $ elements ['a' .. 'z']
--   return (i, randomString, j)

-- arbitraryIOLabeledTransition :: Gen LabeledTransition
-- arbitraryIOLabeledTransition = do
--   i <- arbitrary :: Gen Integer
--   j <- arbitrary :: Gen Integer
--   randomString <- listOf $ elements ['a' .. 'z']
--   randomIO <- elements ["!", "?"]
--   return (i, randomIO ++ randomString, j)

-- arbitraryIO :: Gen Char
-- arbitraryIO = elements ['!', '?']

-- arbitraryIOLabeledTransition = (arbitrary :: Gen LabeledTransition) `suchThat` (\a@(_,x,_) ->  head x == '?' || head x== '!')

arbitraryLabeledTransitions :: Gen [LabeledTransition]
arbitraryLabeledTransitions = listOf1 (arbitrary :: Gen LabeledTransition)

arbitraryIOLabeledTransition :: Gen LabeledTransition
arbitraryIOLabeledTransition = do
  (a, l, b) <- arbitrary :: Gen LabeledTransition
  io <- elements ['!', '?']
  return (a, io : l, b)

arbitraryIOLabeledTransitions :: Gen [LabeledTransition]
arbitraryIOLabeledTransitions = listOf1 arbitraryIOLabeledTransition

arbitraryStates :: Gen [LabeledTransition] -> Gen [State]
arbitraryStates transitions = do
  makeSet . concatMap (\(from, _, to) -> [from, to]) <$> transitions

createIOLTS' :: Gen [LabeledTransition] -> Gen IOLTS
createIOLTS' transitions = do
  (states, labels, transitionSet, initState) <- createLTS' transitions
  return (states, map tail $ filter (\x -> head x == '?') labels, map tail $ filter (\x -> head x == '!') labels, map (\(f, l, t) -> (f, tail l, t)) transitionSet, initState)

createLTS' :: Gen [LabeledTransition] -> Gen LTS
createLTS' transitions = do
  t <- transitions
  states <- arbitraryStates transitions
  return (states, filter (/= tau) $ makeSet (map (\(_, label, _) -> label) t), makeSet t, head states)

arbitraryLTS :: Gen LTS
arbitraryLTS = createLTS' arbitraryLabeledTransitions

arbitraryIOLTS :: Gen IOLTS
arbitraryIOLTS = createIOLTS' arbitraryIOLabeledTransitions

test_validateLTS :: Gen Bool
test_validateLTS = do
  validateLTS <$> arbitraryLTS

exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "Implement ltsGen :: Gen IOLTS && for a non-io LTS"
  putStrLn "Example output: "

_main :: IO ()
_main = do
  exercise2