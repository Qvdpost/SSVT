module Exercise7 where

import Helper(exercise)
import Test.QuickCheck

-- Code start here --

-- Code End here --

exercise7 :: IO ()
exercise7 = do
  putStrLn $ exercise 7 "Luhn's Algorithm"
  -- putStrLn "Solution uses an Int value for `n` and calculates the result!"
  -- putStrLn "calculate::Int->Int"
  -- putStrLn $ "calculate 2 -> " ++ show (calculate 2)

  putStrLn "\n{ \"Property 1\": \"xx\" }"
  putStr "\t"
  -- quickCheck $ prop_a calculate
  putStrLn "\tλ> Discarded tests are negative numbers."

_main :: IO ()
_main = do
  exercise7