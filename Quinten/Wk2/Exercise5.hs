module Exercise5 where

import Data.Char
import Helper (exercise)
import Exercise4 (isPermutation)
import Data.List
import Test.QuickCheck

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement a b = isPermutation a b && and (zipWith (/=) a b)


deran :: Int -> [[Int]]
deran n = [perm | perm <- permutations numbers, isDerangement perm numbers]
  where
    numbers = [0..n-1]


{-
  Properties weak to strong:
  0. Continuation from the list for permutations.
  1. The reverse of even lists is always a derangement, and never for odd sized lists.
  2. 
-}

{-
  Test report:
  Since derangements are firstly permutations, all tests from exercise 4 (about permutations) can be reused.
  Aside frome the property of it being a permutation, a derangement has the property of having no element in it's original position.

  For this, we need to be able to distinguish each element, thus duplicates cannot be take into account; duplicates are considered outside of our domain.

  Testing the correctness of our isDerangement implementation we consider the following tests:
  1. Running pre-compiled test-lists.
  2. Checking if we correctly recognize that the reverse of any even sized list is always a derangement,
      and the reverse of any oddly sized list is never a derangement (the middle element remains in place)
-}

-- We need an actual generator, since QuickCheck would otherwise discard too many tests -.-, for simplicity we use a list of Int since they derive from Eq.
gen_UniqueInts :: Gen [Int]
gen_UniqueInts = (arbitrary :: Gen [Int]) `suchThat` (\xs -> length (nub xs) == length xs)

-- { \as -> even length as = b } result = isDerangement as (reverse as) { result == b } -- domain 2 or more elements
prop_RevDeran :: Eq a => [a] -> Property
prop_RevDeran as = len > 1 && length (nub as) == len ==> even (len) == isDerangement as (reverse as)
  where
    len = length as

exercise5 :: IO ()
exercise5 = do
  -- putStrLn $ exercise 1 "XXX"
    print ("Hi")

_main :: IO ()
_main = do
  exercise5
