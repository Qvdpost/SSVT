{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}
module Exercise4 where

import Helper (exercise)
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

reversal :: Integer -> Integer
reversal = read . reverse . show

revPrimes :: Integer -> [Integer]
revPrimes n = filter (prime . reversal) (takeWhile (< n) primes)

exercise4 :: IO ()
exercise4 = do
  putStrLn $ exercise 4 "Function to find all primes whose reversals are also primes."

  print (revPrimes 10000)

  putStrLn "\n == Comments =="
  putStrLn "\nTesting this function could be done in a number of ways, but most would revolve around using a different approach to find all of the same prime numbers."
  putStrLn "\nDifferent checks would consist of verifying each number is indeed a prime number, and thus be a test of the 'prime' function that was used."
  putStrLn "\nEach of the property tests would not be testing an atom so to say, but rather a function that should have its own tests as well."
  putStrLn "\nThis function assumes that the reversal of a number is allowed to be the same as the number itself."

_main :: IO ()
_main = do
  exercise4