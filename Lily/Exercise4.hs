module Exercise4 where

import Helper (exercise)
import Test.QuickCheck

-- Provided by WorkShop
reversal :: Integer -> Integer
reversal = read . reverse . show

getPrimes :: [Integer]
getPrimes = [i | i <- [3, 5 .. 10000], isPrime i && isPrime (reversal i)]

isPrime :: Integral a => a -> Bool
isPrime x = null [i | i <- [3, 5 .. floor (sqrt (fromIntegral x))], x `mod` i == 0]

-- Not a property?
-- prop_lengthNotMax f x = length (f x) /= 10000

-- fromIntegral needed, solution from here
-- https://stackoverflow.com/questions/6695267/get-sqrt-from-int-in-haskell

exercise4 :: IO ()
exercise4 = do
  putStrLn $ exercise 4 "Write a function that finds all primes < 10000 with its reverse being a prime"
  putStrLn "Solution uses an Int value for `n` and calculates the result!"
  putStrLn "getPrimes :: [Integer]"
  putStrLn "Below can be seen a sample of 7 primes"
  putStrLn $ "getPrimes -> " ++ show (take 7 getPrimes)

--   putStrLn "\n{ \"Property 1\": \"Length should not be 10,000\" }"
--   putStr "\tNot every number between 1 → 10,000 is prime."
--   quickCheck $ prop_a calculate
--   putStrLn "\tλ> Discarded tests are negative numbers."

_main :: IO ()
_main = do
  exercise4