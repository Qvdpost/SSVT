module Exercise4 where

import Data.Char
import Helper (exercise)
import Test.QuickCheck
import Data.List



-- https://stackoverflow.com/questions/2097501/learning-haskell-how-to-remove-an-item-from-a-list-in-haskell
removeItem :: Eq a => a -> [a] -> [a]
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
    ([1,2], [1,2,3]) --part of somelist isperm somelist -> false
  ]

{-
By assuming there are no duplicates in the test lists, testing for permutations is greatly trivialized. It reduces the number of testable properties to the following 2:
The two lists are the same length, and each element in a is present in b.
Since there exists no list that can have an element that is not in the other list, if both lists also contain the same distinct elements and are of the same length, 
  given the fact that there can be no duplicates.
-}

{-
Weak to Strong:
0. Elements in the one list have to be represented in the other list <-> Both lists are the same length (both weak, but incomparable; the left property includes cases not represented in the right property)
1. No list contains elements that are not in the other list.
2. Occurences of distinct items are equal in both lists <- but we don't have to go this far, since there are no duplicates.
-}


-- { pre_cond as bs } result = isPermutation as bs { post_cond result }
pre_condElems :: [Int] -> [Int] -> Bool
pre_condElems as bs = as `elem` (permutations bs)


-- post_condElems :: Property -> Bool
-- post_condElems b = property True == b
prop_ElemsPermutations ::[Int] -> [Int] -> Property
prop_ElemsPermutations as bs = pre_condElems as bs ==> isPermutation as bs == True


-- pre_cond :: Eq a => [a] -> [a] ->
-- pre_cond as bs = as ++ [b | not . elem b bs]
-- pre_condPrepend :: [Int] -> [Int] -> Bool
-- pre_condElems as bs = True

-- gen_Prepend :: Gen ([Int], [Int])
-- gen_Prepend = let as = arbitrary :: Gen [Int] in (as, head (permuations as))



exercise4 :: IO ()
exercise4 = do
  -- putStrLn $ exercise 1 "XXX"
    print ("Hi")
    quickCheck (prop_ElemsPermutations)



_main :: IO ()
_main = do
  exercise4