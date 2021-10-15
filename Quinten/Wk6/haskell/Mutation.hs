module Mutation where
import Test.QuickCheck
import Data.List
import MultiplicationTable

mutate :: ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Integer -> Gen Bool
mutate prop fut input = randomMutator >>= \mutator -> propertyExecutor prop (mutator $ fut input) input

randomMutator :: Gen ([Integer] -> Gen [Integer])
randomMutator = elements mutators

propertyExecutor :: ([Integer] -> Integer -> Bool) -> Gen [Integer] -> Integer -> Gen Bool
propertyExecutor prop o x = o >>= \output -> return $ prop output x

-- Mutators
mutators :: [[Integer] -> Gen [Integer]]
mutators = [addElements, removeElements]

addElements :: [Integer] -> Gen [Integer]
addElements xs = do
  nums <- arbitrary :: Gen [Integer]
  num <- arbitrary :: Gen Integer
  return $ num : xs ++ nums

removeElements :: [Integer] -> Gen [Integer]
removeElements xs = choose (1, length xs) >>= \x -> return $ take x xs