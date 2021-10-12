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
import LTS (LTS, IOLTS, Label, LabeledTransition, State, createLTS, createIOLTS, makeSet, tau)
import Test.QuickCheck (vectorOf,Arbitrary, Gen, Positive, Property, arbitrary, elements, forAll, generate, listOf,listOf1, quickCheck, sample, shuffle, suchThat, (==>))

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

arbitraryLabeledTransitions :: Gen [LabeledTransition]
arbitraryLabeledTransitions = listOf1 (arbitrary :: Gen LabeledTransition)

insertIO :: LabeledTransition -> LabeledTransition -- Gen LabeledTransition
insertIO (a, lt, b) = do
    io <- arbitraryIO
    return (a, [io] ++ lt, b)

arbitraryIOLabeledTransitions:: Gen [LabeledTransition]
arbitraryIOLabeledTransitions = do
    lts <- arbitraryLabeledTransitions
    -- return [(a, [io] ++ lt, b) | (a, lt, b) <-lts | c <- arbitraryIO]
    return (map (\x -> insertIO x) lts)

arbitraryIO :: Gen Char
arbitraryIO = elements ['!', '?']

-- arbitraryIOLabel ::

arbitraryLTS :: Gen LTS
arbitraryLTS = createLTS' arbitraryLabeledTransitions

arbitraryIOLTS :: Gen IOLTS
arbitraryIOLTS = createIOLTS' arbitraryIOLabeledTransitions

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

exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "XX"
  putStrLn "Example output: "

_main :: IO ()
_main = do
  exercise2