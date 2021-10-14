module Mutation where
import Test.QuickCheck
import Data.List
import MultiplicationTable

mutate :: ([Integer] -> Integer -> Property) -> (Integer -> [Integer]) -> Integer -> Property
mutate prop fut input = property (randomMutator >>= \mutator -> return $ propertyExecutor prop (mutator $ fut input) input)

randomMutator :: Gen ([Integer] -> Gen [Integer])
randomMutator = elements mutators

propertyExecutor :: ([Integer] -> Integer -> Property) -> Gen [Integer] -> Integer -> Property
propertyExecutor prop o x = property $ o >>= \output -> return $ prop output x

-- Mutators
mutators :: [[Integer] -> Gen [Integer]]
mutators = [addElements, removeElements]

addElements :: [Integer] -> Gen [Integer]
addElements xs = (++) xs <$> (arbitrary :: Gen [Integer])

removeElements :: [Integer] -> Gen [Integer]
removeElements xs = choose (0, length xs) >>= \x -> return $ take x xs
