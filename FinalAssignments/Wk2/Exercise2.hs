module Exercise2 where

import Data.Char
import Data.List (sort)
import Helper (exercise)
import System.Random
import Test.QuickCheck

data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)
-- 1 Hour 40 minutes
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | not isTriangle = NoTriangle
    | a == b && a == c = Equilateral
    | a == b || a == c || b == c = Isosceles
    | (a^2) + (b^2) == (c^2) = Rectangular
    | otherwise = Other
    -- Triangle Inequality Theorem
    -- https://www.mathsisfun.com/geometry/triangle-inequality-theorem.html
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

exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "Identifying triangle types"
  putStrLn $ "Generate different types of invalid triangles."
  putStrLn $ "Prop Triangle Overextend No Triangle:"
  quickCheck (forAll gen_NoTriangleOverextend prop_NoTriangle)
  putStrLn $ "Prop Triangle Underextend No Triangle:"
  quickCheck (forAll gen_NoTriangleUnderextend prop_NoTriangle)
  putStrLn $ "Prop Triangle No Zero Length:"
  quickCheck (forAll gen_NoZero prop_NoZeroLen)

_main :: IO ()
_main = do
  exercise2