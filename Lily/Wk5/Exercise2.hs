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
import Test.QuickCheck --(sized, Arbitrary, Gen, Positive, Property, arbitrary, elements, forAll, generate, listOf, listOf1, quickCheck, sample, shuffle, suchThat, vectorOf, (==>))

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

-- arbitraryStates :: Gen [LabeledTransition] -> Gen [State]
-- arbitraryStates transitions = do
--   makeSet . concatMap (\(from, _, to) -> [from, to]) <$> transitions

arbitraryLabeledTransition :: Gen LabeledTransition
arbitraryLabeledTransition = do
  i <- (arbitrary :: Gen Integer) `suchThat` (0 <)
  j <- (arbitrary :: Gen Integer) `suchThat` (0 <)
  randomString <- listOf1 $ elements ['a' .. 'z']
  return (i, randomString, j)

arbitraryLabeledTransitions :: Gen [LabeledTransition]
arbitraryLabeledTransitions = listOf1 arbitraryLabeledTransition -- (arbitrary :: Gen LabeledTransition)

arbitraryIOLabeledTransition :: Gen LabeledTransition
arbitraryIOLabeledTransition = do
  (a, l, b) <- arbitraryLabeledTransition -- arbitrary :: Gen LabeledTransition
  io <- elements ['!', '?']
  return (a, io : l, b)

-- containsBothIOs :: Gen [LabeledTransition] -> Gen Bool
-- containsBothIOs ls = do
--     t <- ls
--     return not $ null (map tail $ filter (\x -> head x == '?') getStates t) && not $ null (map tail $ filter (\x -> head x == '!') getStates t)

arbitraryIOLabeledTransitions :: Gen [LabeledTransition]
arbitraryIOLabeledTransitions =
    sized $
        \n -> do
            k <- choose (2, n)
            sequence [ arbitraryIOLabeledTransition | _ <- [1..k]] `suchThat` (\transitions -> any (\(_,label,_) -> head label == '!') transitions && any (\(_,label,_) -> head label == '?') transitions)
    -- listOf1 arbitraryIOLabeledTransition

getStates :: [LabeledTransition] -> [State]
getStates t = makeSet (concatMap (\(from, _, to) -> [from, to]) t)

createIOLTS' :: Gen [LabeledTransition] -> Gen IOLTS
createIOLTS' transitions = do
  (states, labels, transitionSet, initState) <- createLTS' transitions
  return (states, map tail $ filter (\x -> head x == '?') labels, map tail $ filter (\x -> head x == '!') labels, map (\(f, l, t) -> (f, tail l, t)) transitionSet, initState)

createLTS' :: Gen [LabeledTransition] -> Gen LTS
createLTS' transitions = do
  t <- transitions
  return (getStates t, filter (/= tau) $ makeSet (map (\(_, label, _) -> label) t), makeSet t, head (getStates t))

arbitraryLTS :: Gen LTS
arbitraryLTS = createLTS' arbitraryLabeledTransitions

ltsGen :: Gen IOLTS
ltsGen = createIOLTS' arbitraryIOLabeledTransitions

{-
LTS Properties:

1. No duplicate states
2. No duplicate labels
3. No duplicate transitions
4. non-empty set of transitions
5. States of transitions are a sublist of LTS states
6. The initstate is an element of LTS states

-}

-- Duplicating states should result in a false LTS
prop_DuplicateStates :: IOLTS -> Bool
prop_DuplicateStates iolts =
    not $ validateLTS ((\(states, stim_labels, resp_labels, transitions, init) -> (states, stim_labels, resp_labels, transitions, init)) iolts)

-- prop_DuplicateStates (states, stim_labels, resp_labels, transitions, init) =
--     not $ validateLTS ((\(states, stim_labels, resp_labels, transitions, init) -> (states++states, stim_labels, resp_labels, transitions, init)) (createIOLTS transitions))

-- Duplicating all labels should result in a false LTS
prop_DuplicateLabels :: IOLTS -> Bool
prop_DuplicateLabels (states, stim_labels, resp_labels, transitions, init) =
    not $ validateLTS ((\(states, stim_labels, resp_labels, transitions, init) -> (states, stim_labels++stim_labels, resp_labels, transitions, init)) (createIOLTS transitions))

-- Duplicating the transitions should result in a false LTS
prop_DuplicateTransitions :: IOLTS -> Bool
prop_DuplicateTransitions (states, stim_labels, resp_labels, transitions, init) =
    not $ validateLTS ((\(states, stim_labels, resp_labels, transitions, init) -> (states, stim_labels, resp_labels, transitions++transitions, init)) (createIOLTS transitions))

-- Replacing transitions with an empty array should result in a false LTS
prop_EmptyTransitions :: IOLTS -> Bool
prop_EmptyTransitions (states, stim_labels, resp_labels, transitions, init) =
    not $ validateLTS ((\(states, stim_labels, resp_labels, transitions, init) -> (states, stim_labels, resp_labels, [], init)) (createIOLTS transitions))

-- Manipulation of transitions (appending a non-existing state) should result in a false LTS
prop_SublistStates :: IOLTS -> Bool
prop_SublistStates (states, stim_labels, resp_labels, transitions, init) =
    not $ validateLTS ((\(states, stim_labels, resp_labels, transitions, init) -> (states, stim_labels, resp_labels, transitions++[(init, head stim_labels, 0)], init)) (createIOLTS transitions))

-- Manipulation of initState should result in a false LTS
prop_InitIsElement :: IOLTS -> Bool
prop_InitIsElement (states, stim_labels, resp_labels, transitions, init) =
    not $ validateLTS ((\(states, stim_labels, resp_labels, transitions, init) -> (states, stim_labels, resp_labels, transitions, 0)) (createIOLTS transitions))


exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "Implement ltsGen :: Gen IOLTS && for a non-io LTS"

  putStrLn "QuickCheck for valid LTS's: "
  quickCheck (forAll ltsGen validateLTS)

  putStrLn "\nQuickCheck for duplicate state in LTS: "
  quickCheck (forAll ltsGen prop_DuplicateStates)

  putStrLn "\nQuickCheck for duplicate labels in LTS: "
  quickCheck (forAll ltsGen prop_DuplicateLabels)

  putStrLn "\nQuickCheck for duplicate transitions in LTS: "
  quickCheck (forAll ltsGen prop_DuplicateTransitions)

  putStrLn "\nQuickCheck for empty transitions in LTS: "
  quickCheck (forAll ltsGen prop_EmptyTransitions)

  putStrLn "\nQuickCheck for checking transition states to be a sublist of states in LTS: "
  quickCheck (forAll ltsGen prop_SublistStates)

  putStrLn "\nQuickCheck for checking if init is a state in LTS: "
  quickCheck (forAll ltsGen prop_InitIsElement)

_main :: IO ()
_main = do
  exercise2