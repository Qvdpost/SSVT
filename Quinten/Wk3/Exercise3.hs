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

{-
  A boolean expression φ is in conjunctive normal form(CNF) if φ a sum of Cnj clauses (Ci) where each clause
  Ci is the disjunction of zero or more literals.
  Input is presumed to be arrowfree (template copied from Lecture3.hs nnf)

  Inspiration: https://hackage.haskell.org/package/hatt-1.5.0.3/docs/src/Data-Logic-Propositional-NormalForms.html#toCNF
-}
toCNF :: Form -> Form
toCNF (Prop x) = Prop x
toCNF (Neg (Prop x)) = Neg (Prop x)
toCNF (Cnj fs) = Cnj (map toCNF fs)
toCNF (Dsj [f]) = toCNF f -- Dissolve disjunction with 1 clause
toCNF (Dsj (f:fs)) = distr (toCNF f) (toCNF (Dsj fs)) -- Apply distributative law on Disjunctions with multiple forms (recursively on fs)
toCNF (Dsj []) = Cnj [] -- Convert empty disjunction; base case. Should it be a Dsj?

{-
  Distributive law: P∨(Q∧R)) `Equiv` (P∨Q)∧(P∨R)
  
  https://hackage.haskell.org/package/hatt-1.5.0.3/docs/src/Data-Logic-Propositional-NormalForms.html#toCNF
  -- distr (Cnj (f:fs)) g = Cnj (distr f g) (distr g fs) -- Outer conjunction clauses
  -- distr fs (Cnj f g) = Cnj (distr f fs) (distr g fs) -- Outer conjunction clauses
  -- distr f g = Dsj f g -- Inner disjunction clauses
-}
distr :: Form -> Form -> Form
distr (Cnj [f]) g = distr f g -- Dissolve conjuction with 1 clause
distr (Cnj (f:fs)) g = Cnj [distr f g, distr (Cnj fs) g] -- Distribute with 2 clauses, both need to be recurrently distributed
distr (Cnj []) g = Dsj [] -- end of the line empty conjunction
distr f (Cnj [g]) = distr f g -- Dissolve conjuction with 1 clause
distr f (Cnj (g:gs)) = Cnj [distr f g, distr f (Cnj gs)]
distr f (Cnj []) = Dsj []
distr f g = Dsj [f,g] -- Termination clause


-- Definition of CNF should come here.
-- "A statement is in conjunctive normal form if it is a conjunction (sequence of ANDs) consisting of one or more conjuncts, each of which is a disjunction (OR) of one or more literals"
-- https://mathworld.wolfram.com/ConjunctiveNormalForm.html
formToCNF :: Form -> Form
formToCNF f = toCNF (nnf (arrowfree f))

{-- Test Cases
  P                             "1" No Change
  !P                            "-1" No Change
  (P v Q) ∧ (!P v R)            "*(+(1 2) +(-1 2))" No Change
  A v B                         "1+2" No Change
  P -> Q   -> -P ∨ Q            "(1==>2)" -> +(-1 2)
  !!P                           "--1" -> "1"
  !(P ^ Q) -> (!P ∨ !Q)         "-(1*2)" -> "+(-1 -2)"
  !(P <--> Q)                   "-(1 <=>2)" -> "*(+(-1 -2) +(1 2))"
  P ∨ (Q ∧ R)                   "+(1 *(2 3))"


  Maybe..
  (P -> Q) ^ (P -> R) Requires two steps to break down    (1==>2)*(1==>2)

--}



exercise3 :: IO ()
exercise3 = do
  putStrLn $ exercise 3 "Converting Formulas to CNF"
  putStrLn $ "Example output: P v (Q ^ R) is turned into (->) (P v Q) ^ (P v R)"
  putStrLn $ "In lexer language: +(1 *(2 3)) is turned into (->) *(+(1 2) +(1 3))"

  putStrLn $ "Test (input -> output): P -> P"
  print (formToCNF (fst((parseForm(lexer "1"))!!0)))
  putStrLn $ "\n"

  putStrLn $ "Test: !P -> !P"
  print (formToCNF (fst((parseForm(lexer "-1"))!!0)))
  putStrLn $ "\n"

  putStrLn $ "Test: (P v Q) ^ (!P v R) -> (P v Q) ^ (!P v R)"
  print (formToCNF (fst((parseForm(lexer "*(+(1 2) +(-1 2))"))!!0)))
  putStrLn $ "\n"

  putStrLn $ "Test: A v B -> A v B"
  print (formToCNF (fst((parseForm(lexer "1+2"))!!0)))
  putStrLn $ "\n"

  putStrLn $ "Test: P -> Q  -> -P v Q"
  print (formToCNF (fst((parseForm(lexer "(1==>2)"))!!0)))

  putStrLn $ "Test: !!P"
  print (formToCNF (fst((parseForm(lexer "--1"))!!0)))
  putStrLn $ "\n"

  putStrLn $ "Test: !(P ^ Q) -> (!P v !Q)"
  print (formToCNF (fst((parseForm(lexer "-*(1 2)"))!!0)))
  putStrLn $ "\n"

  putStrLn $ "Test: !(P <--> Q)"
  print (formToCNF (fst((parseForm(lexer "-(1 <=>2)"))!!0)))
  putStrLn $ "\n"

  putStrLn $ "Test: P v (Q ^ R) -> (P v Q) ^ (P v R)"
  print (formToCNF (fst((parseForm(lexer "+(1 *(2 3))"))!!0)))



_main :: IO ()
_main = do
  exercise3