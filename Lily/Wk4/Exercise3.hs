{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

-- Time Spent: 
module Exercise3 where

import Data.List
import Helper (exercise)
import System.Random
import Test.QuickCheck

exercise3 :: IO ()
exercise3 = do
  putStrLn $ exercise 3 "Implement binary relations as list of pairs"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise3