{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}
module Exercise5 where

import Helper (exercise)
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3 ..]

sumBunch = sum (take 101 primes)

-- This function should be provided with the sum of the first 101 primes, so that it can
-- dynamically update the sum, instead of re-calculating each recursive step.
primes101 :: [Integer] -> Integer -> Integer
primes101 (x : xs) n
  | prime n = n
  | otherwise = primes101 nextPrimes newSum
  where
    newPrime = xs !! 100 -- Since the head is already removed from xs; index 100 is the followup prime.
    nextPrimes = xs ++ [newPrime]
    newSum = n - x + newPrime

exercise5 :: IO ()
exercise5 = do
  putStrLn $ exercise 5 "Function to find the smallest prime that is a sum of 101 consecutive primes."

  print (primes101 primes sumBunch)

  putStrLn "\n == Comments =="
  putStrLn "\nTesting the correctness of the function would be writing the inverse of the function itself."
  putStrLn "\nThe output would be taken and then you'd look for a smaller prime than than number and see if the preceding 101 prime numbers sums up to that target."
  putStrLn "\nSince the primes function is already supposed to do that, testing this property redundant."
  putStrLn "\nYou could however test wether a similar function with an alternative implementation gives the same result. Then you'd be cover some possible programming errors, though it would probably best to not use the same helper functions in that case."
  putStrLn "\nThat function would need to be tested too though. For which you would need another function, and you would go down a recursive test rabbit hole."

_main :: IO ()
_main = do
  exercise5