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
import Test.QuickCheck

factorial :: Int -> Int
factorial n
  | n < 0 = error "negative factorial"
  | n == 0 = 1
  | otherwise = n * factorial (n -1)

testPermutations :: Int -> Property
testPermutations n = n >= 0 && n < 10 ==> length (permutations as) == count
  where
    as = [1 .. n]
    count = factorial n

exercise3 :: IO ()
exercise3 = do
  putStrLn $ exercise 3 "Test that the closed formula of permutations of a list equals length factorial of that list."

  putStrLn "\n== Check length of permutations is length! of list (Success) =="
  putStrLn "Input/Output space coverage: Only feasible (<10) natural numbers as input are covered."
  quickCheck testPermutations

  putStrLn "\n == Comments =="
  putStrLn "\nThe complexity of testing that the number of permutations for a sequence of length n is n!."
  putStrLn "\nThis problem scales horribly and quickly becomes way too large to test thoroughly."
  putStrLn "\n"
  putStrLn "\nRight now we're testing wether permutations produces a list as long as it's supposed to. Not wether the elements inside actually represent all the permutations."
  putStrLn "\nBesides, the tests only cover a miniscule part of the entire domain."

_main :: IO ()
_main = do
  exercise3