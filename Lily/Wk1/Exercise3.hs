module Exercise3 where

import Helper(exercise)

exercise3 :: IO ()
exercise3 = do
  putStrLn $ exercise 3 "Exercise 5 of Workshop 1, testing the property for integer lists of the form [1..n]."
  putStrLn "This property is computationally expensive to test."
  putStrLn "Specifically it is O(n!) making it computationally infeasible to thoroughly test"


_main :: IO ()
_main = do
  exercise3