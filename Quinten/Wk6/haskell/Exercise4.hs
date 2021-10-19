{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise4 where

import Data.List
import Helper (exercise)
import System.Random
import Test.QuickCheck
import Exercise2
import Exercise3
import MultiplicationTable
import Mutation
import Data.List
--Time Spent:

strengthProps :: [[Integer] -> Integer -> Bool] -> [[Integer]] -> Integer -> String
strengthProps props mutations input
  | strength < 0.11 = "Weak low : " ++ show strength
  | strength < 0.33 = "Mild low : " ++ show strength
  | strength < 0.66 = "Strong 💪: " ++ show strength
  | strength < 0.88 = "Mild high : " ++ show strength
  | otherwise = "Weak high : " ++ show strength
  where
    strength = fromIntegral survivors / fromIntegral total_mutations
    results = testMutations mutations props input
    total_mutations = length results
    survivors = total_mutations - length (filter (==False) results)


exercise4 :: IO ()
exercise4 = do

  putStrLn $ exercise 4 "Find the strength of a given set of properties, a percentage of killed mutants"
  testInt <- generate (arbitrary :: Gen Integer)
  mutations <- generate $ genMutants 100 multiplicationTable testInt
  print $ strengthProps allProps mutations testInt

  putStrLn "\n====Comments===="
  putStrLn "\nThe number of survivors (mutants that do not get killed by any property test) divided by all generated mutants is converted into the scale as presented in the FitSpec paper."

_main :: IO ()
_main =
  exercise4