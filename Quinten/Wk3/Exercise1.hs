{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise1 where

import Data.List
import Helper (exercise)
import Lecture3 (allVals,evl,Form(Impl,Equiv))
import System.Random
import Test.QuickCheck

-- By definition a contradiction must evaluate all values to False
contradiction :: Form -> Bool
contradiction f = and [not (evl x f)  | x <- allVals f]

-- Like above, except must evaluate to True
tautology :: Form -> Bool
tautology f = and [  evl x f  | x <- allVals f]

-- | logical entailment
-- I haven't heard of this before, following this
-- https://www.quora.com/What-is-tautological-entailment
entails :: Form -> Form -> Bool
entails f ff = tautology (Impl f ff)

-- | logical equivalence
-- Assuming by name, this is from the above
equiv :: Form -> Form -> Bool
equiv f ff = tautology (Equiv f ff)

{-
  Procedure:
  Contradictions properties:
    - For no permutation of the literals in the form does the form evaluate to true
    - The form requires at least 1 of either a Conjunction, Equivalence or Implication, else there can be no conflict of clauses.
  
  Tautology properties:
    - For no permutation of the literals in the form does the form evaluate to false
    - 

  Entailment properties:
    - 
    - 
  
  Equivalence properties:
    - 
    - 

    
-}

exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "Definitions of: contradiction,tautology,entails,equiv"

_main :: IO ()
_main = exercise1