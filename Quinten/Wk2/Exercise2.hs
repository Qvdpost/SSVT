module Lab2 where

import Data.Char
import Data.List (sort)
import Helper (exercise, tupleToList)
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

gen_NoTriangle :: Gen (Positive Integer, Positive Integer, Positive Integer)
gen_NoTriangle = (arbitrary :: Positive Integer, arbitrary:: Positive Integer, arbitrary :: Positive Integer) `suchThat` (\a b c -> minimum [a, b, c] < 10)

-- For every triplet where the shortest side is shorter than the difference between the other sides, the triplet does not represent a triangle (no intersection between radiuses)
prop_NoTriangle :: Integer -> Integer -> Integer -> Bool
prop_NoTriangle a b c = triangle a b c == NoTriangle

exercise1 :: IO ()
exercise1 = do
  -- putStrLn $ exercise 1 "XXX"
  print (triangle 1 2 3)
  putStrLn "Hi"

_main :: IO ()
_main = do
  exercise1