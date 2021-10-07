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
import SetOrd (Set (Set),inSet,unionSet,subSet, list2set)
import System.Random
import Test.QuickCheck

type Rel a = [(a,a)]


remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x ys@(y:ys') = if x == y then ys' else y : remove x ys'

-- [a] may not contain duplicates
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial as ((a,b):rels) | a /= b = isSerial (remove b as) rels
                         | otherwise = isSerial as rels
isSerial [] _ = True
isSerial as [] = False

newtype Domain a = Domain [a] deriving (Eq)

instance (Show a) => Show (Domain a) where
  showsPrec _ (Domain s) str = showDom s str

showDom []     str = showString "{}" str
showDom (x:xs) str = showChar '{' ( shows x ( showl xs str))
  where
    showl []     str = showChar '}' str
    showl (x:xs) str = showChar ',' (shows x (showl xs str))

list2dom :: Eq a => [a] -> Domain a
list2dom [] = Domain []
list2dom xs = Domain xs

instance (Arbitrary a, Eq a) => Arbitrary (Domain a) where
  arbitrary = list2dom . nub <$> listOf arbitrary


isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation (a:as) bs | a `elem` bs = isPermutation as (remove a bs)
                        | otherwise = False
isPermutation [] [] = True

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement a b = isPermutation a b && and (zipWith (/=) a b)

deran :: Eq a => [a] -> [a]
deran as = (take 1 [perm | perm <- permutations as, isDerangement perm as]) !! 0

generateRelsDeranged :: Eq a => Domain a -> Rel a
generateRelsDeranged (Domain as) = zip (as) (deran as)

generateRelsWithSelf :: Eq a => Domain a -> Rel a
generateRelsWithSelf (Domain as) = zip (as) (as)

prop_DerangedIsSerial :: Domain Int -> Property
prop_DerangedIsSerial (Domain as) = length as > 1 && length as < 10 ==> isSerial as (generateRelsDeranged (Domain as)) == True

prop_SelfIsNotSerial :: Domain Int -> Property
prop_SelfIsNotSerial (Domain as) = length as > 1 ==> isSerial as (generateRelsWithSelf (Domain as)) == False

exercise4 :: IO ()
exercise4 = do
  putStrLn $ exercise 4 "Testing isSerial"

  putStrLn "\n== Test for isSerial on a relation of the domain and its deranged self =="
  putStrLn "Input/Output space coverage: Random domains of size 2 - 10 are tested. The relation generated is the first permutation that is a derangement. This relation should always be serial."
  quickCheck prop_DerangedIsSerial


  putStrLn "\n== Test for isSerial on a relation of the domain and itself =="
  putStrLn "Input/Output space coverage: Random domains of larger than 2 are tested. The relation generated is of each atom to itself. This should never be a derangement since there are only xRx relations."
  quickCheck prop_SelfIsNotSerial


  putStrLn "\n == Comments =="
  putStrLn "\nBut we do show that a list of unique relations that use all atoms is always serial. This can be verified by the fact that every atom is used only once on the left and once on the right side of a relation and there are no relations with itself. If it would not be serial, an atom would not be present on the right side, that is present on the left side. That would mean there is an element present in the domain that is not present in its derangement, or is used in a xRx relation. But that would not be possible because of the derangement mapping and thus: Absurdity." 
  putStrLn "\nWe also show that exclusively xRx relations are never serial and our implementation correctly identifies those."
  putStrLn "\nThe selfIsNotSerial test actually lead us to a an error in our implementation where we forgot to use the guard to check for a != b. Testing 1 Us 0."
  putStrLn "\nThere's also the case where atoms in a domain are not used at all in a relation, but we have no tests for that. Leaving quite a gap in our testing procedure. But with our given tests we believe that we partially cover the functionality of our code."


  putStrLn "\n == Question 3 =="
  putStrLn "\nThe use of mod implies a domain of positive Integers. Though there is a modulo for floats it is not often used."
  putStrLn "\n"

--   n = 1
--   [
--     (0, 5) -> 0 = 5 % 1
--     (x, 0) -> x = 0 % 1 
--   ]

_main :: IO ()
_main = do
  exercise4