module Lab2 where

import Data.Char
import Helper (exercise)
import Exercise4 (isPermutation)
import Data.List


isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement a b = isPermutation a b && and (zipWith (/=) a b)


deran :: Int -> [[Int]]
deran n = [perm | perm <- permutations numbers, isDerangement perm numbers]
  where
    numbers = [0..n-1]

-- isDerangement [] [] == False 
-- vs
-- isPermutation [] [] == True
{-
Properties:
1. { \as -> even length as = b } result = isDerangement as (reverse as) { result == b }
2. 
-}

exercise5 :: IO ()
exercise5 = do
  -- putStrLn $ exercise 1 "XXX"
    print ("Hi")

_main :: IO ()
_main = do
  exercise5
