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
import Lecture3
import System.Random
import Test.QuickCheck
import Lecture3


{-
  Properties (weak to strong):
    - Successful parsing generates a non-empty list of forms.
    - Showing the returned value of successful parsing should be equivalent to the input value.

  Note:
    Parse discards redundant closing brackets. Part of props??
-}

prop_ParseNonEmpty :: String -> Bool
prop_ParseNonEmpty s = not (null (parse s))

prop_ParseInputEquivOutput :: String -> Bool
prop_ParseInputEquivOutput s = "[" ++ s ++ "]" == show (parse s) 

exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "Testing Lecture Parse Function"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise2