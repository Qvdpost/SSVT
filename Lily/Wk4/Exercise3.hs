{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

-- Time Spent: 
module Exercise3 where

import Data.List
import Helper (exercise)
import System.Random
import Test.QuickCheck


type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos = concatMap (\(a,b) -> [(a, b), (b, a)])

-- Test that checks if symClos produces a pre-computed result
testSymClos :: Ord a => Rel a -> Rel a -> Bool
testSymClos as bs = symClos as == bs


exercise3 :: IO ()
exercise3 = do
  putStrLn $ exercise 3 "Implement binary relations as list of pairs"

  putStrLn "\n== Test symClos =="
  putStrLn "Input/Output space coverage: [(1,2),(2,3),(3,4)] -> [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)] -> True."
  print (testSymClos [(1,2),(2,3),(3,4)] [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)])

_main :: IO ()
_main = do
  exercise3