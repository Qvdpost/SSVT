{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise6 where

import Data.List
import Exercise5 (Rel, trClos, (@@))
import Exercise4 (Domain(Domain))
import Exercise3 (symClos)
import Helper (exercise)
import System.Random
import Test.QuickCheck

-- Time Spent: 25 minutes

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

subsetOf :: Ord a => Rel a -> Rel a -> Bool
subsetOf r trRs = all (`elem` trRs) r

isSymmetric :: Ord a => Rel a -> Bool
isSymmetric ((a, b):(c, d):rels) = (a == d && b == c) && isSymmetric rels
isSymmetric ((a, b):rels) = False
isSymmetric [] = True

isTransitive :: Ord a => Rel a -> Bool
isTransitive rels = (rels @@ rels) `subsetOf` rels

-- Test that checks if symClos produces a pre-computed result
testSymClos :: Ord a => Rel a -> Rel a -> Bool
testSymClos as bs = symClos as == bs

-- A relation is a subset of a symmetric closure applied to it
prop_symClosSubset :: Ord a => Rel a -> Bool
prop_symClosSubset rels = rels `subsetOf` (symClos rels)

-- A test to check if a result from symClos is symmetric
prop_symClosSymmetrical :: Ord a => Rel a -> Bool
prop_symClosSymmetrical rels = isSymmetric (symClos rels)

-- A relation is a subset of a transitive closure applied to it
prop_trClosSubset :: Ord a => Rel a -> Bool
prop_trClosSubset rels = rels `subsetOf` (trClos rels)

-- A test to ensure the result of trClos is transitive
prop_trClosTransitive :: Ord a => Rel a -> Bool
prop_trClosTransitive rels = isTransitive (trClos rels)

-- Generate an arbitary amount of Relations
arbitrarySizedRel :: Int -> Gen [(Gen Int, Gen Int)]
arbitrarySizedRel n = do
  k <- choose (0, n)
  (Domain as) <- arbitrary :: Gen (Domain Int)
  return [ (elements as, elements as) | _ <- [1..k] ]


-- -- Based on arbitraryList from https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
-- arbitraryRels :: Arbitrary a => Domain a -> Gen [(a, a)]
-- arbitraryRels =
--   sized $
--     \n d -> do
--       k <- choose (0, n)
--       sequence [ (elements d, elements d) | _ <- [1..k] ]

exercise6 :: IO ()
exercise6 = do
  putStrLn $ exercise 6 "Test Exercise 5 & 3"

  putStrLn "\n== Test symClos on pre-computed input/output =="
  putStrLn "Input/Output space coverage: [(1,2),(2,3),(3,4)] -> [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)] -> True."
  print (testSymClos [(1,2),(2,3),(3,4)] [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)])


  putStrLn "\n == Comments =="
  putStrLn "\nQuickCheck could be used if we could figure out in time how to generate relations. For a random relation on a random domain, that relation should be a subset of its symmetrical/transitive closure."
  putStrLn "\nBesides that property, each can be tested by a specific test. isSymmetrical and isTransitive for respectively symClos and trClos."
  putStrLn "\nFinally we would have tested whether they produced a 'smallest' relation. Removing any element from the trClos or symClos produced relation should break their respective property of isTransitive/isSymmetrical."

_main :: IO ()
_main = do
  exercise6