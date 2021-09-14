
module Lab1 where
import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Q1
-- Calculate the sum of squares
sumofsquaresleft :: Integer -> Integer
sumofsquaresleft n | n > 0 = sum [k^2 | k <- [1..n] ]
                  | n <= 0 = 0
-- Calculate the right on the equal sign
sumofsquaresright :: Integer -> Integer
sumofsquaresright n | n > 0 = (n * (n + 1) *(2*n + 1)) `div` 6
                  | n <= 0 = 0
-- Determine if they are equal
sumofsquares :: Integer -> Bool
sumofsquares n = sumofsquaresright n == sumofsquaresleft n

-- Calculate the sum of squares
sumofcubicleft :: Integer -> Integer
sumofcubicleft n | n > 0 = sum [k^3 | k <- [1..n] ]
                  | n <= 0 = 0
-- Calculate the right on the equal sign
sumofcubicright :: Integer -> Integer
sumofcubicright n | n > 0 = ((n * (n +1)) `div` 2)^2
                  | n <= 0 = 0
-- Determine if they are equal
sumofcubic :: Integer -> Bool
sumofcubic n = sumofcubicright n == sumofcubicleft n

-- Q2
-- Calculate the number of set
numberofset :: [Integer] -> Integer
numberofset n = toInteger(length n)
-- Calculate the number of subset
numberofset2 :: [Integer] -> Integer
numberofset2 n = toInteger(length (subsequences n))
--Whether their relationship is normal
numberofset3 :: [Integer] -> Bool
numberofset3 n = 2 ^ (numberofset n) == numberofset2 n
--The difficulty is that some sets are so large during QuickCheck
-- that the testing process gets stuck. It's hard to know if your
-- program actually passed.

-- Q3
-- Compute the permutation in terms of a given function
permutationnum :: [Integer] -> Integer
permutationnum n = toInteger(length(permutations n))
-- Calculate the factorial of the function
permutationnum2 :: [Integer] -> Integer
permutationnum2 n = toInteger (product [1..(length n)])
--Whether their relationship is equal
permutationnum3 :: [Integer] -> Bool
permutationnum3 n = permutationnum n == permutationnum2 n

-- Q4
primenum :: Integer -> [Integer]
primenum n = filter(\x -> (prime x)&&(prime (reversal x))&&(x>10))[1..n]

-- It seems like that the two number of q4 is different, but maybe I am overthinking


primenum2 :: Integer -> [Integer]
primenum2 n = filter(prime.reversal)(takeWhile( <= n)primes)
primenum3 n = filter(>=10)(primenum2 n)

primenum4 :: Integer -> Bool
primenum4 n = primenum n == primenum3 n
-- Q5
