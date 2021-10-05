{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise2 where

import Data.List (intersect, nub, (\\), union, sort)
import Helper (exercise)
import SetOrd (Set (Set),inSet,unionSet,subSet)
import Test.QuickCheck ( (==>), Property, Gen, generate, arbitrary, suchThat, quickCheck, forAll)
import Exercise1(generateSetSystemRandom, generateSetQuickCheck)

-- Time Spent: 1 Hour 30 minutes

setIntersection::Eq a => Set a -> Set a -> Set a
setIntersection (Set a) (Set b) = Set (nub (a `intersect` b))

setUnion:: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

setDifference::Eq a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = Set (a \\ b)

setEquals :: Ord a => Set a -> Set a -> Bool
setEquals (Set a) (Set b) = sort a == sort b

--  Homebrew-ed Tests  --
testIntersection :: IO Bool
testIntersection = do
    (Set a) <- generateSetSystemRandom
    (Set b) <- generateSetSystemRandom
    return (setIntersection (Set a) (Set b) == Set (a `intersect` b))

testUnion :: IO Bool
testUnion = do
    (Set a) <- generateSetSystemRandom
    (Set b) <- generateSetSystemRandom
    return (setUnion (Set a) (Set b) == Set (a `union` b))

testDifference :: IO Bool
testDifference = do
    (Set a) <- generateSetSystemRandom
    (Set b) <- generateSetSystemRandom
    return (setDifference (Set a) (Set b) == Set (a \\ b))
--  End  --

--  QuickCheck Tests  --
testIntersection' :: Set Int -> Set Int -> Bool
testIntersection' (Set a) (Set b) = setIntersection (Set a) (Set b) == Set (a `intersect` b)

testUnion' :: Set Int -> Set Int -> Bool
testUnion' (Set a) (Set b) = unionA `setEquals` unionB
    where
        unionA = setUnion (Set a) (Set b)
        unionB = Set (a `union` b)

testDifference' :: Set Int -> Set Int -> Bool
testDifference' (Set a) (Set b) = setDifference (Set a) (Set b) == Set (a \\ b)
--  End  --

exercise2 :: IO ()
exercise2 = do
    putStrLn $ exercise 2 "Implement and test set Intersection, Union, Difference"
    putStrLn "Example output: "

    putStrLn "\n== Test intersections =="
    putStrLn "Input/Output space coverage: Random sets."
    quickCheck testIntersection'

    putStrLn "\n== Test Union =="
    putStrLn "Input/Output space coverage: Random sets."
    quickCheck testUnion'

    putStrLn "\n== Test difference =="
    putStrLn "Input/Output space coverage: Random sets."
    quickCheck testDifference'

    putStrLn "\n == Comments =="
    putStrLn "\n"
    putStrLn "All tests were validated using 'known to work' default Haskell implementations."


_main :: IO ()
_main = do
    exercise2