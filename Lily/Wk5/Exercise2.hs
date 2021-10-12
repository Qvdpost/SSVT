{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise2 where

import Data.List (intersect, nub, sort, union, (\\))
import Helper (exercise)
import LTS (LTS, Label, LabeledTransition, State, createLTS, makeSet, tau)
import Test.QuickCheck (Arbitrary, Gen, Positive, Property, arbitrary, elements, forAll, generate, listOf,listOf1, quickCheck, sample, shuffle, suchThat, (==>))

-- Time Spent:

-- ltsGen :: Gen IOLTS
-- ltsGen =

-- ([State], [Label], [Label], [LabeledTransition], State)

arbitraryLabeledTransition :: Gen LabeledTransition
arbitraryLabeledTransition = do
  i <- arbitrary :: Gen Integer
  j <- arbitrary :: Gen Integer
  randomString <- listOf $ elements ['a' .. 'z'] --"abcdefghijklmnopqrstuvwxyz"
  return (i, randomString, j)

arbitraryLabeledTransitions :: Gen [LabeledTransition]
arbitraryLabeledTransitions = listOf1 (arbitrary :: Gen LabeledTransition)

arbitraryLTS :: Gen LTS
arbitraryLTS = createLTS' arbitraryLabeledTransitions

arbitraryStates :: Gen [LabeledTransition] -> Gen [State]
arbitraryStates transitions = do
    labeledTransitions <- transitions
    return (makeSet (concatMap (\(from, _, to) -> [from, to]) labeledTransitions))

createLTS' :: Gen [LabeledTransition] -> Gen LTS
createLTS' transitions = do
  t <- transitions
  states <- arbitraryStates transitions
  return (states, filter (/= tau) $ makeSet (map (\(_, label, _) -> label) t), makeSet t, head states)

exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "XX"
  putStrLn "Example output: "

_main :: IO ()
_main = do
  exercise2