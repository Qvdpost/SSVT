module Exercise6 where

import Helper(exercise)

reversal :: Integer -> Integer
reversal = read . reverse . show

getPrimes ::Integer-> [Integer]
getPrimes n = [i | i <- [2 ..n], isPrime i]

isPrime :: Integral a => a -> Bool
isPrime x = null [i | i <- [2 .. ceiling (sqrt (fromIntegral x -1))], x `mod` i == 0]

validate::Integer -> Bool
validate n = isPrime (product (getPrimes n) + 1)

-- Helper function for _ref n []
-- Initialise the recursion
refute::Integer -> [Integer]
refute n = _ref n []

_ref::Integer->[Integer]->[Integer]
_ref n xs
  | n == 0 = xs
  | valid = _ref (n-1) (n:xs)
  | otherwise = _ref (n-1) xs
  where valid= not (validate n)

exercise6 :: IO ()
exercise6 = do
  putStrLn $ exercise 6 "Write a refute to primes from 2 to n multiplied + 1 being prime"
  putStrLn "Checks the product of the primes + 1 to be prime"
  putStrLn "refute::Integer -> Bool"
  putStrLn $ "refute -> " ++ show (refute 20)

-- fromIntegral needed, solution from here
-- https://stackoverflow.com/questions/6695267/get-sqrt-from-int-in-haskell

_main :: IO ()
_main = do
  exercise6