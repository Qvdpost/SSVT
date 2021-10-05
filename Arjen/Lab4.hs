module Lab4 where

import Data.List
import Control.Monad (foldM, replicateM)
import System.Random
import Test.QuickCheck
import SetOrd

-- | Assignment 1: 45 min.

-- Generating a random set of integers from scratch.
genRandomIntList :: Int -> IO [Int]
genRandomIntList n = replicateM n $ randomRIO (0, 9)

genRandomIntSet :: IO (Set Int)
genRandomIntSet = genRandomIntList 10 >>= \a -> return $ list2set a

-- Generating a random set of integers using QuickCheck.
-- Used by the QuickCheck library to generate random data.
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = list2set <$> arbitrary

-- This function can be used to fill the set with random integers.
genRandomIntSet' :: Gen (Set Integer)
genRandomIntSet' = listOf (choose (0, 9)) >>= \a -> return $ list2set a

-- | Assignment 2: 160 min.

setIntersect :: (Ord a) => Set a -> Set a -> Set a
setIntersect (Set xs) (Set ys) = list2set $ xs `intersect` ys

setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set xs) (Set ys) = list2set $ xs `union` ys

setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set xs) (Set ys) = list2set $ xs \\ ys

{-
    To test the union, intersection and difference functions we
    use the basic set theory.
    Lets define two sets A and B, then the following property should hold:
    - The union of the intersection of A and B and the difference of A and B
    should equal the set A.
    - The union of the intersection of A and B and the difference of B and A
    should equal the set B.
    We test the functions and see if they abide the same logics of set theory.
    These observations are drawn from Venn diagrams.
-}

propTestA :: (Ord a) => Set a -> Set a -> Bool
propTestA xs ys = (xs `setIntersect` ys) `setUnion` (xs `setDifference` ys) == xs

propTestB :: (Ord a) => Set a -> Set a -> Bool
propTestB xs ys = (xs `setIntersect` ys) `setUnion` (ys `setDifference` xs) == ys

-- This function will test properties A and B for one instance of two randomly generated sets
testSetProperty :: IO Bool
testSetProperty = do
    xs   <- genRandomIntSet
    ys   <- genRandomIntSet
    let resA = propTestA xs ys
    let resB = propTestB xs ys

    return $ resA && resB

-- Use this function to thorougly test. It will use 100 random instances of sets
-- to test properties A and B.
testSetProperties :: IO ()
testSetProperties = do
    tests <- replicateM 100 testSetProperty

    if and tests
        then print "pass on all tests"
        else print "failed some tests"

-- These are tests using QuickCheck. To test for more datatypes,
-- simply run quickCheck propTestA, or quickCheck propTestB
quickCheckTestA, quickCheckTestB :: IO ()
quickCheckTestA = quickCheck (forAll genRandomIntSet' propTestA)
quickCheckTestB = quickCheck (forAll genRandomIntSet' propTestB)

-- | Assignment 3: 20 min

type Rel a = [(a, a)]

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

symClos :: Ord a => Rel a -> Rel a
symClos (x:xs) = x : swap x : symClos xs
symClos x      = x

-- | Assignment 4: 300 mins
--isSerial :: Eq a => [a] -> Rel a -> Bool
--isSerial a rel = not implemented

-- The answer could not be implemented. I have included a short explanation of how I
-- would implement it here. Let's assume the input is [1,2,3,4] [(1,1),(1,3),(2,4),(3,2)].
-- First take the head of the domain A, in this case 1. Find all the tuples in the relation
-- with 1 as the x element, so (1,1) and (1,3). If there are none return false. Then check if
-- the y elements of the relation with that x are in A, so 1 and 3, which they are. If not
-- return false. Then repeat for all elements of A. If none are false return True.
-- In this example False would be returned since the element 4 is not an x value in any
-- of the relations.


-- | Assignment 5: 60 mins

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
   nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos a =  if (a `union` (a @@ a) == a) then sort a
            else trClos ((a @@ a) `union` a)
           
-- | Assignment 6:

newtype Element = Element Int deriving (Eq,Ord,Show)
instance Arbitrary Element where
    arbitrary = oneof (map (return.Element) [0..10])

            
-- | Assignment 7: 10 mins

--Proof by counterexample:

--symClos (trClos [(1,2),(2,3),(3,4)])
--[(1,2),(2,1),(1,3),(3,1),(1,4),(4,1),(2,3),(3,2),(2,4),(4,2),(3,4),(4,3)]

--trClos (symClos [(1,2),(2,3),(3,4)])
--[(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)]

-- So the counterexample shows that there is a difference between
-- the  symmetric closure of the transitive closure of a relation R
-- and the transitive closure of the symmetric closure of R
-- because symClos (trClos [(1,2),(2,3),(3,4)])
-- does not equal trClos (symClos [(1,2),(2,3),(3,4)]).
