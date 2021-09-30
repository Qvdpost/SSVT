{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise2 where

import Data.List (intersect, nub, (\\))
import Helper (exercise)
import SetOrd (Set (Set),inSet,unionSet)
import Test.QuickCheck

setIntersection::Eq a => Set a -> Set a -> Set a
setIntersection (Set a) (Set b) = Set (nub (a `intersect` b))

setUnion:: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

setDifference::Eq a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = Set (a \\ b)

genNoDuplicates:: Gen Set [Int]
genNoDuplicates = Set (generate arbitrary :: IO [Int] `suchThat` $ \x -> x == nub x)

testIntersection :: Set [Int] -> Set [Int] -> Bool
testIntersection (Set a) (Set b) = setIntersection (Set a) (Set b) == set (a `interact` b)

testUnion :: Set [Int] -> Set [Int] -> Bool
testUnion (Set a) (Set b) = setUnion (Set a) (Set b) == set (unionBy (==) a b)

testDifference :: Set [Int] -> Set [Int] -> Bool
testDifference (Set a) (Set b) = setDifference (Set a) (Set b) == Set (a \\ b)

exercise2 :: IO ()
exercise2 = do
    putStrLn $ exercise 2 "Implement and test set Intersection, Union, Difference"
    putStrLn $ "Example output: "
    quickCheck genNoDuplicates testIntersection
    quickCheck genNoDuplicates testUnion
    quickCheck genNoDuplicates testDifference

_main :: IO ()
_main = do
    exercise2