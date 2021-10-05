{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise6 where

import Data.List
import Exercise5 (Rel, trClos)
import Helper (exercise)
import System.Random
import Test.QuickCheck

-- Time Spent: 25 minutes

subsetOf :: Ord a => Rel a -> Rel a -> Bool
subsetOf r trRs = all (`elem` trRs) r

prop_subset :: Ord a => Rel a -> Bool
prop_subset r = r `subsetOf` trClos r

exercise6 :: IO ()
exercise6 = do
  putStrLn $ exercise 6 "Test Exercise 5"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise6