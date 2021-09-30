{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise2 where

import Data.List (intersect, nub, (\\), union)
import Helper (exercise)
import SetOrd (Set (Set),inSet,unionSet)
import Test.QuickCheck ( (==>), Property, Gen, generate, arbitrary, suchThat, quickCheck)
import Exercise1(generateSetSystemRandom)

-- Time Spent: 1 Hour 30 minutes

setIntersection::Eq a => Set a -> Set a -> Set a
setIntersection (Set a) (Set b) = Set (nub (a `intersect` b))

setUnion:: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

setDifference::Eq a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = Set (a \\ b)

--  Homebrew-ed Tests  --
testIntersection :: Bool
testIntersection = do
    (Set a) <- generateSetSystemRandom
    (Set b) <- generateSetSystemRandom
    return (setIntersection (Set a) (Set b) == Set (a `intersect` b))

testUnion :: Bool
testUnion = do
    (Set a) <- generateSetSystemRandom
    (Set b) <- generateSetSystemRandom
    return (setUnion (Set a) (Set b) == Set (a `union` b))

testDifference :: Bool
testDifference = do
    (Set a) <- generateSetSystemRandom
    (Set b) <- generateSetSystemRandom
    return (setDifference (Set a) (Set b) == Set (a \\ b))
--  End  --

--  QuickCheck Tests  --
testIntersection' :: Set Int -> Set Int -> Bool
testIntersection' (Set a) (Set b) = setIntersection (Set a) (Set b) == Set (a `intersect` b)

testUnion' :: Set Int -> Set Int -> Bool
testUnion' (Set a) (Set b) = setUnion (Set a) (Set b) == Set (a `union` b)

testDifference' :: Set Int -> Set Int -> Bool
testDifference' (Set a) (Set b) = setDifference (Set a) (Set b) == Set (a \\ b)
--  End  --

exercise2 :: IO ()
exercise2 = do
    putStrLn $ exercise 2 "Implement and test set Intersection, Union, Difference"
    putStrLn $ "Example output: "
    -- quickCheck genNoDuplicates testIntersection
    -- quickCheck genNoDuplicates testUnion
    -- quickCheck genNoDuplicates testDifference

_main :: IO ()
_main = do
    exercise2