{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise6 where

import Data.List
import Helper (exercise)
import Lecture3
import System.Random
import Test.QuickCheck

exercise6 :: IO ()
exercise6 = do
  putStrLn $ exercise 6 "Converting formulas to clause form"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise6