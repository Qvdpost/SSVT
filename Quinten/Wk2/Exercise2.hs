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

-- Generates tuples such that the smallest integer is smaller than the absolute difference between the other integers
gen_NoTriangleOverextend :: Gen (Integer, Integer, Integer)
gen_NoTriangleOverextend = (arbitrary :: Gen (Integer, Integer, Integer)) `suchThat` (\(a,b,c) -> let sorted = sort [a, b, c] in head sorted < last sorted - head (tail sorted) )

-- Generates tuples such that the smallest integer is smaller than the absolute difference between the other integers
gen_NoTriangleUnderextend :: Gen (Integer, Integer, Integer)
gen_NoTriangleUnderextend = (arbitrary :: Gen (Integer, Integer, Integer)) `suchThat` (\(a,b,c) -> let sorted = sort [a, b, c] in last sorted > head sorted + head (tail sorted) )


-- For every triplet where the shortest side is shorter than the difference between the other sides, the triplet does not represent a triangle (no intersection between radiuses of smallest and longest)
-- For every triplet where the longest side is longer than the others combined the radiuses of the two short ends don't intersect and there can be no triangle
prop_NoTriangle :: (Integer, Integer, Integer) -> Bool
prop_NoTriangle (a,b,c) = triangle a b c == NoTriangle


gen_NoZero :: Gen (Integer, Integer, Integer)
gen_NoZero = (arbitrary :: Gen (Integer, Integer, Integer)) `suchThat` (\(a,b,c) -> 0 `elem` [a,b,c])

-- No triangle can have a side with length zero
prop_NoZeroLen :: (Integer, Integer, Integer) -> Bool
prop_NoZeroLen (a, b, c) = triangle a b c == NoTriangle

exercise1 :: IO ()
exercise1 = do
  -- putStrLn $ exercise 1 "XXX"
  print (triangle 1 2 3)
  putStrLn "Hi"

_main :: IO ()
_main = do
  exercise1