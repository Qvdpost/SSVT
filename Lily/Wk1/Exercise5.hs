module Exercise5 where

import Helper (exercise)
import Test.QuickCheck

getPrimes ::Integer-> [Integer]
getPrimes n = [i | i <- [3, 5 .. n], isPrime i]

isPrime :: Integral a => a -> Bool
isPrime x = null [i | i <- [3, 5 .. floor (sqrt (fromIntegral x))], x `mod` i == 0]

x n = sum (getPrimes n)

-- x::Int->Int
-- x n = head (filter (prime.sum) (process 101 (getPrimes (n)))
-- Not a property?
-- prop_lengthNotMax f x = length (f x) /= 10000

-- fromIntegral needed, solution from here
-- https://stackoverflow.com/questions/6695267/get-sqrt-from-int-in-haskell

exercise5 :: IO ()
exercise5 = do
  putStrLn $ exercise 5 "Write a function that finds all primes < 10000 with its reverse being a prime"
  putStrLn "Solution uses an Int value for `n` and calculates the result!"
  putStrLn "getPrimes :: [Integer]"
  putStrLn "Below can be seen a sample of 7 primes"
  putStrLn $ "getPrimes -> " ++ show (take 7 (getPrimes 20))

--   putStrLn "\n{ \"Property 1\": \"Length should not be 10,000\" }"
--   putStr "\tNot every number between 1 → 10,000 is prime."
--   quickCheck $ prop_a calculate
--   putStrLn "\tλ> Discarded tests are negative numbers."

_main :: IO ()
_main = do
  exercise5