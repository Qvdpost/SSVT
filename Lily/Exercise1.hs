module Exercise1 where

import Helper(exercise)
import Test.QuickCheck

calculate :: Int -> Int
calculate n = (n * (n + 1) * ((2 * n) + 1)) `div` 6

calculateB :: Int -> Int
calculateB n = ((n*(n+1)) `div` 2) ^ 2

prop_a :: (Int -> Int) -> Int -> Property
prop_a f n = (n > 0) ==> f n == sum [i ^ 2 | i <- [1 .. n]]

prop_b :: (Int -> Int) -> Int -> Property
prop_b f n = (n > 0) ==> f n == sum [i ^ 3 | i <- [1 .. n]]

exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "A. Natural numbers 1²,2²,3²..n² sum to ⁿ⁽ⁿ⁺¹⁾⁽²ⁿ⁺¹⁾⁄₆"
  putStrLn "Solution uses an Int value for `n` and calculates the result!"
  putStrLn "calculate::Int->Int"
  putStrLn $ "calculate 2 -> " ++ show (calculate 2)

  putStrLn "\n{ \"Property 1\": \"xx\" }"
  putStr "\t"
  quickCheck $ prop_a calculate
  putStrLn "\tλ> Discarded tests are negative numbers."

exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "A. Natural numbers 1³,2³,3³..n³ sum to (ⁿ⁽ⁿ⁺¹⁾⁄₂)²"
  putStrLn "Solution uses an Int value for `n` and calculates the result!"
  putStrLn "calculateB::Int->Int"
  putStrLn $ "calculateB 5 -> " ++ show (calculateB 5)

  putStrLn "\n{ \"Property 1\": \"xx\" }"
  putStr "\t"
  quickCheck $ prop_b calculateB
  putStrLn "\tλ> Discarded tests are negative numbers."

_main :: IO ()
_main = do
  exercise1
  exercise2