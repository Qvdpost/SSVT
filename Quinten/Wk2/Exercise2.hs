module Lab2 where

import Data.Char
import Data.List (sort)
import Helper (exercise)
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | not isTriangle = NoTriangle
    | a == b && a == c = Equilateral
    | a == b || a == c || b == c = Isosceles
    | (a^2) + (b^2) == (c^2) = Rectangular
    | otherwise = Other
    -- Triangle Inequality Theorem below
    where isTriangle = (a + b) > c && (a + c) > b && (b + c) > a 



exercise1 :: IO ()
exercise1 = do
  -- putStrLn $ exercise 1 "XXX"
  print (triangle 1 2 3)
  putStrLn "Hi"

_main :: IO ()
_main = do
  exercise1