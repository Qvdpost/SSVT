module Lab2 where

import Data.Char
import Data.List (sort)
import Helper (exercise)
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)
-- c = (oldC + n) % 26
rotate::String->Int->String
rotate s n= (((ord s)+n ) `mod` 26)+97


exercise3 :: IO ()
exercise3 = do
  -- putStrLn $ exercise 1 "XXX"
  print (triangle 1 2 3)
  putStrLn "Hi"

_main :: IO ()
_main = do
  exercise1