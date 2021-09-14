module Exercise6 where

import Helper(exercise)

reversal :: Integer -> Integer
reversal = read . reverse . show

getPrimes ::Integer-> [Integer]
getPrimes n = [i | i <- [2 ..n], isPrime i]

isPrime :: Integral a => a -> Bool
isPrime x = null [i | i <- [2 .. ceiling (sqrt (fromIntegral x -1))], x `mod` i == 0]

refute::Integer -> Bool
refute n = isPrime (product (getPrimes n) + 1)

exercise6 :: IO ()
exercise6 = do
  putStrLn $ exercise 6 "Write a refute to primes from 2 to n multiplied + 1 being prime"
  putStrLn "Checks the product of the primes +1 to be prime"
  putStrLn "refute::Integer -> Bool"
  putStrLn $ "refute -> " ++ show (refute 13)

-- fromIntegral needed, solution from here
-- https://stackoverflow.com/questions/6695267/get-sqrt-from-int-in-haskell

_main :: IO ()
_main = do
  exercise6