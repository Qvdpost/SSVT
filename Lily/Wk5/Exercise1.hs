{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

-- Time Spent: 30 Minutes

module Exercise1 (_main, validateLTS) where

import Data.List
import Helper (exercise)
import LTS
import System.Random (Random (randomR), getStdRandom)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate, listOf, sample)

--Time Spent:

{--
 (Q,L,T,q)
 -- Q is a countable, non-empty list;
 -- L is a countable set of labels;
 -- T ⊆ Q×(L∪{τ})×Q, with τ ∈/ L, is the transition relation;
 -- q0 ∈ Q is the initial state
--}

subsetOf :: Ord a => [a] -> [a] -> Bool
subsetOf r trRs = all (`elem` trRs) r

-- ([State], [Label], [LabeledTransition], State)
-- validateLTS :: LTS -> Bool
-- validateLTS ([],_,_,_) = False
-- validateLTS (q,l,(a,t,b),q_zero) | length l > 100 = False
--                                  | length q > 100 = False
--                                  | not (a `elem` q && t `elem` l && b `elem` q) = False
--                                  | not (q_zero `elem` q) = False
--                                  | otherwise = True

-- validateLTS :: LTS -> Bool
-- validateLTS ([],_,_,_) = False
-- validateLTS (q,l,[(a,t,b)],q_zero) | length l > 100 = False
--                                  | length q > 100 = False
--                                  | not (a `elem` q && t `elem` l && b `elem` q) = False
--                                  | notElem q_zero q = False
--                                  | otherwise = True
-- validateLTS (_,_,[],_) = False

validateLTS :: IOLTS -> Bool
validateLTS (states, stimulus_labels, response_labels, labeledTransitions, startState) =
  states /= []
    && not (hasDuplicates states)
    && not (hasDuplicates stimulus_labels || hasDuplicates response_labels)
    && not (hasDuplicates labeledTransitions)
    && not (null labeledTransitions)
    && checkTransitions (states, stimulus_labels, response_labels, labeledTransitions, startState) (createIOLTS labeledTransitions)

checkTransitions :: IOLTS -> IOLTS -> Bool
checkTransitions (states1, stim_labels1, resp_labels1, _, startState1) (states2, stim_labels2, resp_labels2, _, startState2) =
  subList states2 (sort states1)
    && subList (filter (/= "tau") stim_labels2) (sort stim_labels1)
    && subList (filter (/= "tau") resp_labels2) (sort resp_labels1)
    && subList [startState1] states1

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList _ [] = False
subList [] _ = True
subList (x : xs) (y : ys)
  | x == y = subList xs ys
  | otherwise = subList (x : xs) ys

exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "Implement Validate LTS"
  putStrLn "Example output: "
  putStr "tretmanK1: "
  print (validateLTS tretmanK1)

  putStr "tretmanK2: "
  print (validateLTS tretmanK2)

  putStr "tretmanK3: "
  print (validateLTS tretmanK3)

  putStr "tretmanI1: "
  print (validateLTS tretmanI1)

  putStr "tretmanS1: "
  print (validateLTS tretmanS1)

  putStr "coffeeImpl1: "
  print (validateLTS coffeeImpl1)

_main :: IO ()
_main = do
  exercise1