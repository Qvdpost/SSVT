{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

-- Time Spent: 30 Minutes

module Exercise1 (_main) where

import Data.List (nub)
import Helper (exercise)
import System.Random (Random (randomR), getStdRandom)
import Test.QuickCheck (Arbitrary (arbitrary), generate, Gen, sample, listOf)
import LTS (LTS)
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
validateLTS :: LTS -> Bool
validateLTS ([],_,_,_) = False
validateLTS (q,l,(a,t,b),q_zero) | length l > 100 = False
                                 | length q > 100 = False
                                 | not (a `elem` q && t `elem` l && b `elem` q) = False
                                 | not (q_zero `elem` q) = False
                                 | otherwise = True

exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "XX"
  putStrLn "Example output: "

_main :: IO ()
_main = do
  exercise1