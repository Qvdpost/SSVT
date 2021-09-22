{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise3 where

import Data.List
import Helper (exercise)
import Lecture3
import System.Random
import Test.QuickCheck

exercise3 :: IO ()
exercise3 = do
  putStrLn $ exercise 3 "Converting Formulas to CNF"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise3