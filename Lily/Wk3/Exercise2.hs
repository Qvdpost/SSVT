{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise2 where

import Data.List
import Helper (exercise)
import Lecture3(parse)
import System.Random ()
import Test.QuickCheck



exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "Testing Lecture Parse Function"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise2