module Exercise1 where

import Helper(exercise)
import Test.QuickCheck

-- Code start here --

-- Code End here --

exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "A. Natural numbers 1²,2²,3²..n² sum to ⁿ⁽ⁿ⁺¹⁾⁽²ⁿ⁺¹⁾⁄₆"
  putStrLn $ exercise 1 "B. Natural numbers 1³,2³,3³..n³ sum to (ⁿ⁽ⁿ⁺¹⁾⁄₂)²"
  -- putStrLn "Solution uses an Int value for `n` and calculates the result!"
  -- putStrLn "calculate::Int->Int"
  -- putStrLn $ "calculate 2 -> " ++ show (calculate 2)

  putStrLn "\n{ \"Property 1\": \"xx\" }"
  putStr "\t"
  -- quickCheck $ prop_a calculate
  putStrLn "\tλ> Discarded tests are negative numbers."

_main :: IO ()
_main = do
  exercise1