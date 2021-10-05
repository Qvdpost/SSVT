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
remove x ys@(y:ys') = case x == y of
                        True -> ys'
                        _  -> y : remove x ys'

-- [a] may not contain duplicates
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial as ((a,b):rels) = isSerial (remove b as) rels
isSerial [] _ = True
isSerial as [] = False


exercise4 :: IO ()
exercise4 = do
  putStrLn $ exercise 4 "Using Random Testing Methods test previous Exercise"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise4