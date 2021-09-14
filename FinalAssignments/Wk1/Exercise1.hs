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
import Test.QuickCheck

powerSum :: Integer -> Integer -> Integer
powerSum m n = sum $ map (^ m) [1 .. n]

quadraSum :: Integer -> Integer
quadraSum n = (n * (n + 1) * (2 * n + 1)) `div` 6

thirdSum :: Integer -> Integer
thirdSum n = (n * (n + 1) `div` 2) ^ 2

testQuadraSum :: Integer -> Property
testQuadraSum n = n >= 0 ==> powerSum 2 n == quadraSum n

testBaseQuadraSum :: Bool
testBaseQuadraSum = quadraSum 1 == 1

testThirdSum :: Integer -> Property
testThirdSum n = n >= 0 ==> powerSum 3 n == thirdSum n

testBaseThirdSum :: Bool
testBaseThirdSum = thirdSum 1 == 1

exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "Calculate sum of a number-sequence for every number to the power of m"

  putStrLn $ "Example output: " ++ show (quadraSum 10)

  putStrLn "\n== Base case test of n = 1 for the quadratic sum =="
  putStrLn "Input/Output space coverage: Only number 1 as input is covered."
  print testBaseQuadraSum

  putStrLn "\n== Proof induction for quadratic sum (Success) =="
  putStrLn "Input/Output space coverage: Only natural numbers as input are covered."
  quickCheck testQuadraSum

  putStrLn "\n== Base case test of n = 1 for the power of three sum =="
  putStrLn "Input/Output space coverage: Only number 1 as input is covered."
  print testBaseThirdSum

  putStrLn "\n== Proof induction for power of three sum (Success) =="
  putStrLn "Input/Output space coverage: Only natural numbers as input are covered."
  quickCheck testThirdSum

_main :: IO ()
_main = do
  exercise1