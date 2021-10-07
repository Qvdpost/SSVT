{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

-- Time Spent: 15 minutes
module Exercise3 where

import Data.List
import Helper (exercise)
import System.Random
import Test.QuickCheck


type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos = concatMap (\(a,b) -> [(a, b), (b, a)])

exercise3 :: IO ()
exercise3 = do
  putStrLn $ exercise 3 "Implement symClos :: Ord a => Rel a -> Rel a"
  putStrLn $ "Example output: (Input: [(1,2),(2,3),(3,4)])"
  print (symClos [(1,2),(2,3),(3,4)])

_main :: IO ()
_main = do
  exercise3