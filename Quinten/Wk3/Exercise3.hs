{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise3 where

import Data.List
import Helper (exercise)
import Lecture3
import System.Random
import Test.QuickCheck

{-
  Step 1: -> Remove arrows
  Step 2: Convert to nnf
  Step 3: Apply morgan and distributive laws
  Step 4: Profit???
-}

-- Input is presumed to be arrowfree (template copied from Lecture3.hs nnf)
-- Inspiration: https://hackage.haskell.org/package/hatt-1.5.0.3/docs/src/Data-Logic-Propositional-NormalForms.html#toCNF
toCNF :: Form -> Form
toCNF (Prop x) = Prop x
toCNF (Neg (Prop x)) = Neg (Prop x)
toCNF (Cnj fs) = Cnj (map toCNF fs)
toCNF (Dsj (f:fs)) = distr (toCNF f) (map toCNF fs)

{- 
  Distributive law: P∨(Q∧R))`Equiv`(P∨Q)∧(P∨R)

  A boolean expression φ is in conjunctive normal form(CNF) if φ a sum of Cnj clauses (Ci) where each clause
  Ci is the disjunction of zero or more literals.
-}
distr :: Form -> Form -> Form -- The call comes from of toCNF Dsj (f:fs)
distr (Cnj f g) fs = Cnj (distr f fs) (distr g fs) -- Outer conjunction clauses
distr fs (Cnj f g) = Cnj (distr f fs) (distr g fs) -- Outer conjunction clauses
distr f g = Dsj f g -- Inner disjunction clauses

-- Definition of CNF should come here.
-- "A statement is in conjunctive normal form if it is a conjunction (sequence of ANDs) consisting of one or more conjuncts, each of which is a disjunction (OR) of one or more literals"
-- https://mathworld.wolfram.com/ConjunctiveNormalForm.html
formToCNF :: Form -> Form
formToCNF f = toCNF (nnf (arrowfree f))

{-- Test Cases
  P                   No Change
  !P                  No Change
  (P v Q) ^ (!P v R)  No Change
  A v B               No Change
  P -> Q
  !!P
  !(P ^ Q)
  !(P <--> Q)


  Maybe..
  (P -> Q) ^ (P -> R) Requires two steps to break down

--}



exercise3 :: IO ()
exercise3 = do
  putStrLn $ exercise 3 "Converting Formulas to CNF"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise3