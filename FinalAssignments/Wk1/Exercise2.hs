module Exercise2 where

import Helper(exercise)
import Test.QuickCheck

-- Code start here --

-- Code End here --

exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "exercise 4 of Workshop 1"
  -- putStrLn "Solution uses an Int value for `n` and calculates the result!"
  -- putStrLn "calculate::Int->Int"
  -- putStrLn $ "calculate 2 -> " ++ show (calculate 2)

  putStrLn "\n{ \"Property 1\": \"xx\" }"
  putStr "\t"
  -- quickCheck $ prop_a calculate
  putStrLn "\tλ> Discarded tests are negative numbers."

_main :: IO ()
_main = do
  exercise2