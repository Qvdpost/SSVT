{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

-- Time Spent: 
-- {-# LANGUAGE ParallelListComp #-}
module Exercise3 where

import Data.List
import Helper (exercise)
import System.Random
import Test.QuickCheck
import Exercise2
import MultiplicationTable
import Mutation
import Data.List

--Time Spent:

-- Generate a singular mutant
genMutant :: (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> Integer -> Gen [Integer]
genMutant fut mutator input = mutator $ fut input

-- Generate all mutants for a fut
genMutants :: Integer -> (Integer -> [Integer]) -> Integer -> Gen [[Integer]]
genMutants mutants fut input = sequence $ concat [[genMutant fut mutator input | _ <- [1..mutants]] | mutator <- allMutators]

testMutations :: [[Integer]] -> [[Integer] -> Integer -> Bool] -> Integer -> [Bool]
testMutations mutations props input = [False `elem` ([prop output input | prop <- props ]) | output <- mutations]

equivProps :: [[Integer] -> Integer -> Bool] -> [[Integer] -> Integer -> Bool] -> [[Integer]] -> Integer -> Bool
equivProps props others mutations input = testMutations mutations props input == testMutations mutations others input

numberedProps :: [(Int, [Integer] -> Integer -> Bool)]
numberedProps = zip [1..] allProps

namedProps :: [(String, [Integer] -> Integer -> Bool)]
namedProps = zip propNames allProps

minimalProps :: Integer ->(Integer -> [Integer]) -> Integer -> Gen [[String]]
minimalProps mutants fut input = do
  mutations <- genMutants mutants fut input
  return $ take 1 [[index | (index, _) <- subProp] | subProp <- subsequences namedProps, equivProps allProps [prop | (_,prop) <- subProp] mutations input]

exercise3 :: IO ()
exercise3 = do
  putStrLn $ exercise 3 "Calculate the minimal property subset of a 'fut' and a set of properties"

  putStrLn "\nExample Output allMutants: mutants:1 fut:multiplicationTable input:3"
  mutations <- generate $ genMutants 1 multiplicationTable 3
  print mutations

  putStrLn "\nExample Output minimalProps: "
  testInt <- generate (arbitrary :: Gen Integer)
  minProps <- generate $ minimalProps 4000 multiplicationTable testInt
  print minProps

  putStrLn "\n====Comments===="
  putStrLn "\nThe minimal property set generated is based on the apparent equivalence of the subsets. Thus based on the mutants generated the minimal property set that is found can change."
  putStrLn "\nApparent equivalence is determined by checking if the same mutant that is killed by property set A is also killed by property set B. If for every mutant that is teh case, then the two sets are apparently equivalent."


_main :: IO ()
_main =
  exercise3