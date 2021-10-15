{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise2 where

import Helper (exercise)
import Test.QuickCheck
import Mutation
import MultiplicationTable

-- Time Spent:
mutate' :: ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> Integer -> Gen Bool
mutate' prop fut mutator input = propertyExecutor prop (mutator $ fut input) input

countSurvivors :: Int -> [[Integer] -> Gen [Integer]] -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> IO Int
countSurvivors mutants mutators prop fut = tests >>= \x -> return $ length $ filter (== True) x
  where
    tests = generate $ vectorOf mutants (mutate (head props) fut 3)

exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "Implement ltsGen :: Gen IOLTS && for a non-io LTS"

  putStrLn "Example Output: "
  survivors <- countSurvivors 4000 [addElements] [prop_firstElementIsInput] multiplicationTable
  print survivors

_main :: IO ()
_main = do
  exercise2