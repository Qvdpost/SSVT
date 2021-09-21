module Lab2 where

import Data.Char
import Helper (exercise)

-- https://stackoverflow.com/questions/2097501/learning-haskell-how-to-remove-an-item-from-a-list-in-haskell
remove :: Eq a => [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = ys -- Only remove 1 item though.
                    | otherwise = y : removeItem x ys

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation (a:as) bs | a `elem` bs = isPermutation as (removeItem a bs)
                        | otherwise = False
isPermutation [] [] = True

--Testlists and their predicted outcomes.
testLists = [
  ([], []), --emptylist isperm emptylist -> true
  ([], [1,2,3]), --empty list isperm somelist -> false
  ([1,2,3], []), --somelist isperm empty list -> false
  ([1,2,3], [(-1), (-2), (-3)]), --somelist isperm negative somelist -> false
  ([3,2,1], [1,2,3]), --somelist isperm somelist -> true
  ([1,2,3], [1,2]), --somelist isperm part of somelist -> false
  ([1,2], [1,2,3]), --part of somelist isperm somelist -> false
]

{-
Weak to Strong:
0. Elements of a have to be represented in b <-> Same length of both lists (both weak, but incomparable; the left property includes cases not represented in the right property)
1. Both lists contain the same distinct elements
3. No list contains elements that are not in the other list.
2. Occurences of distinct items are equal in both lists <- but we don't have to go this far, since there are no duplicates.
-}

exercise4 :: IO ()
exercise4 = do
  -- putStrLn $ exercise 1 "XXX"
    print ("Hi")

_main :: IO ()
_main = do
  exercise4