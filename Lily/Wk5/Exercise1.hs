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

validateLTS :: LTS -> Bool
validateLTS (states, labels, labeledTransitions, startState) =
  states /= []
    && not (hasDuplicates states)
    && not (hasDuplicates labels)
    && not (hasDuplicates labeledTransitions)
    && (null labeledTransitions || checkTransitions (states, labels, labeledTransitions, startState) (createLTS labeledTransitions))

checkTransitions :: LTS -> LTS -> Bool
checkTransitions (states1, labels1, _, startState1) (states2, labels2, _, startState2) =
  subList states2 (sort states1)
    && subList (filter (/= "tau") labels2) (sort labels1)
    && subList [startState2] states1

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
  putStr "tretmanP: "
  print (validateLTS tretmanP)

  putStr "tretmanQ: "
  print (validateLTS tretmanQ)

  putStr "tretmanR: "
  print (validateLTS tretmanR)

  putStr "tretmanU: "
  print (validateLTS tretmanU)

  putStr "tretmanV: "
  print (validateLTS tretmanV)

  putStr "coffeeImplSimple: "
  print (validateLTS coffeeImplSimple)

_main :: IO ()
_main = do
  exercise1