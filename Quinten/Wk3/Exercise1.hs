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
import Lecture3
import System.Random
import Test.QuickCheck


contradiction :: Form -> Bool

tautology :: Form -> Bool

-- | logical entailment 
entails :: Form -> Form -> Bool

-- | logical equivalence
equiv :: Form -> Form -> Bool

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
  putStrLn $ "Example output: "

_main :: IO ()
_main = exercise1