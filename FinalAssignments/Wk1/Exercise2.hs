{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise2 where

import Data.List
import Helper (exercise)
import Test.QuickCheck

testExactSubsetLength :: Integer -> Property
testExactSubsetLength n = n >= 0 && n < 25 ==> count == length (take count (subsequences as))
  where
    as = [1 .. n]
    count = 2 ^ length as

testPlusOneSubsetLength :: Integer -> Property
testPlusOneSubsetLength n = n >= 0 && n < 25 ==> count == length (take count (subsequences as))
  where
    as = [1 .. n]
    count = 2 ^ length as + 1

exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "Test that a list with length n has 2^n subsets."

  putStrLn "\n== Check length of subsets is 2^length of list (Success) =="
  putStrLn "Input/Output space coverage: Only feasible (<25) natural numbers as input are covered."
  quickCheck testExactSubsetLength

  putStrLn "\n== Check that the length of subsets can never reach 2^length of list + 1 (Fail) =="
  putStrLn "Input/Output space coverage: Only natural numbers as input are covered."
  quickCheck testPlusOneSubsetLength

  putStrLn "\n == Comments =="
  putStrLn "\nThe complexity of testing that the number of subsequences for a sequence of length n is 2^n."
  putStrLn "\nEvery increment to n doubles the amount of work previously required to do. Leading to rapidly increasing runtimes for tests with n larger than the arbitrary number 25."
  putStrLn "\n"
  putStrLn "\nRight now we're testing wether subsequences produces a list as long as it's supposed to. Not wether the elements inside actually represent the powerset."
  putStrLn "\nBesides, the tests only cover a tiny part of the entire domain, testing the entire domain is not feasible. Even testing a small part of the domain is not feasible (domain being all positive numbers)."

_main :: IO ()
_main = do
  exercise2