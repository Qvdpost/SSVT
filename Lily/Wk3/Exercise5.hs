{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise5 where

import Data.List
import Helper (exercise)
import Lecture3
import System.Random
import Test.QuickCheck

exercise5 :: IO ()
exercise5 = do
  putStrLn $ exercise 5 "Implement nsub"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise5