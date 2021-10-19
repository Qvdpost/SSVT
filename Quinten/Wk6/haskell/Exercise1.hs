{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

-- Time Spent: 30 Minutes

module Exercise1 where

import Data.List
import Helper (exercise)
import Test.QuickCheck

--Time Spent:
repeatMutator :: [Integer] -> Gen [Integer]
repeatMutator xs = do
    n <- choose(1, 10)
    return (take (n * length xs) (cycle xs))

chainMutator :: [Integer] -> Gen [Integer]
chainMutator xs = choose(1, 10) >>= \limit -> return $ xs ++ [last xs + (head xs * n) | n <- [1..limit]]

flipSignMutator :: [Integer] -> [Integer]
flipSignMutator xs = map (*(-1)) xs

subsectionMutator :: [Integer] -> Gen [Integer]
subsectionMutator xs = do
  start <- choose (0, length xs - 1)
  end <- choose (start, length xs)
  n <- (arbitrary :: Gen [Integer]) `suchThat` (\x -> length x >= end - start)
  return (take start xs ++ take (end - start) n ++ drop end xs)

exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "Mutators and output types"


  putStrLn "The current mutators (addElement & removeElement) do not manipulate the original values in the list, only the length. Respectively there are arbitrary values added to the original list and a subset is take from the original list. But the original list would still be a subset of the mutated output."

  putStrLn "Mutations where the size of the list is left intact but for which is each element is changed are not tested."

  putStrLn "Methodically mutating the relation of each consecutive number for example could be tested, or leaving the relations intact but with different values would be a viable test."

  putStrLn "\n === Possible mutators ==="
  putStrLn "1. Reverse"
  putStrLn "2. Shift values"
  putStrLn "3. Interleave odds/evens"
  putStrLn "4. Mutate random value"
  putStrLn "5. Mutate subsection"
  putStrLn "6. Accumulate values (leave 0s and add their value to random other element)"
  putStrLn "7. Repeat output n times"
  putStrLn "8. Continue the chain (add the first element to the last element n times)"




  -- Implement Q8, 01,04,07

_main :: IO ()
_main = do
  exercise1