{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise1 where

import Data.List
import Helper (exercise)
import Lecture3
import System.Random
import Test.QuickCheck

exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "Definitions of: contradiction,tautology,entails,equiv"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise1