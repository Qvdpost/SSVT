{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}
module Exercise6 where

import Helper (exercise)
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3 ..]

conjecture :: [Integer] -> Bool
conjecture [] = error "Empty sum is not covered in the conjecture."
conjecture ns = prime (product ns + 1)

testConjecture :: Int -> Property
testConjecture n = n > 0 ==> conjecture (take n primes)

-- This function provides a infinitely increasing list of counterexamples for the conjecture, starting with the smallest example.
testConjectureLinearInf :: Int -> Int -> [[Integer]]
testConjectureLinearInf n m
  | n > 0 = if not (conjecture newPrimes) then [newPrimes] ++ testConjectureLinearInf (n -1) (m + 1) else testConjectureLinearInf n (m + 1)
  | otherwise = []
  where
    newPrimes = take m primes

exercise6 :: IO ()
exercise6 = do
  putStrLn $ exercise 6 "Disproves a conjecture by providing counterexamples that fail to hold true."

  putStrLn "\n== Test conjecture (Fail) =="
  putStrLn "Input/Output space coverage: Natural numbers greater than 1 are considered in the conjecture and in the test."
  quickCheck testConjecture

  putStrLn "\n== Provides a list of counterexamples sorted from smallest to largest  =="
  putStrLn "Input/Output space coverage: A list of length n can be specified and counterexamples starting with size m can be specified."
  print (testConjectureLinearInf 3 1)

  putStrLn "\n == Comments =="
  putStrLn "\nQuickTesting this conjecture has an obvious shortcoming in the fact that it could (depending on the random number generator) generate 100 test cases of which none are an actual counterexample."
  putStrLn "\nLinearly going through each possible counterexample eliminates that possibility, but comes at the disadvantage of not reaching certain test cases until very late."

_main :: IO ()
_main = do
  exercise6