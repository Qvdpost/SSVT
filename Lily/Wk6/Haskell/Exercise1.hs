{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

-- Time Spent: 30 Minutes

module Exercise1 (_main) where

import Data.List
import Helper (exercise)
import Test.QuickCheck

--Time Spent:

exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "Implement Validate LTS"

  putStrLn "Both mutation functions applied, addElements and removeElements, "
  putStrLn "keep the input predominantly as a subset of the original input."
  putStrLn "Unless removeElements chooses to remove all elements."
  putStrLn "\tThis would be considered a weak property."
  putStrLn ""

  putStrLn "-- Properties not covered by these mutators --"
  putStrLn ""
  putStrLn "-- Additional Mutators --"
  putStrLn "\t 1. Add/Subtract/Multiply/Divide/Mod an arbitrary number to each element in the list"
  putStrLn "\t 2. Rotate/Shift elements in the list"
  putStrLn "\t 3. Randomly shuffling the elements"
  putStrLn "\t 4. Negating elements in the list"
  putStrLn "\t 5. Remove elements at the start (reverse take)"
  putStrLn "\t 6. Round numbers to the nearest X value."
  putStrLn "\t 7. Square root every number"
  putStrLn "\t 8. Shift internal digits (123 -> 312)"
  putStrLn "\t 9. Raise each number to the power of itself"
  putStrLn "\t 10. Repeat the list of elements X amount of times"
  putStrLn "\t 11. Randomly slice the list"
  putStrLn "\t 12. Filter by predicate (Eg: All even numbers)"
  putStrLn "\t 13. Append all permutations/combinations of the list to itself"
  putStrLn "\t 14. Absolute all elements in the list"
  putStrLn "\t 15. Cube-root every element"

reverseMutator::[Int] -> [Int]
reverseMutator = reverse

mutateRandomValueMutator'::[Int] -> Gen [Int]
mutateRandomValueMutator' xs = do
  i <- choose(0, length xs)
  newNumber <- (arbitrary :: Gen Int)
  return (take i xs ++ [newNumber] ++ drop i xs)

-- mutateRandomValueMutator :: [Int] -> Gen [Int]
-- mutateRandomValueMutator xs = (choose (0, length xs), arbitrary :: Int) >>= \(i, x) -> return $ (take (i -1) xs) : x : (reverse (take (i -1) (reverse xs)))

_main :: IO ()
_main =
  exercise1