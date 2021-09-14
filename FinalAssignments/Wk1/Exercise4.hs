module Exercise4 where

import Helper(exercise)
import Test.QuickCheck

-- Code start here --

-- Code End here --

exercise4 :: IO ()
exercise4 = do
  putStrLn $ exercise 4 "Write a function that finds all primes < 10000 where the reverse is also a prime"
  -- putStrLn "Solution uses an Int value for `n` and calculates the result!"
  -- putStrLn "calculate::Int->Int"
  -- putStrLn $ "calculate 2 -> " ++ show (calculate 2)

  putStrLn "\n{ \"Property 1\": \"xx\" }"
  putStr "\t"
  -- quickCheck $ prop_a calculate
  putStrLn "\tλ> Discarded tests are negative numbers."

_main :: IO ()
_main = do
  exercise4