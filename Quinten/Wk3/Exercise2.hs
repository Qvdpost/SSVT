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
  putStrLn $ "Example output: parse '*(3 2)' -> [*(3 2)]"

  putStrLn "\n== Test a valid form to parse into a non-empty list. =="
  putStrLn "Input/Output: *(+(1 2) +(-1 2)) -> True."
  print (prop_ParseNonEmpty "*(+(1 2) +(-1 2))")

  putStrLn "\n== Test a valid form to parse to an equivalent list of forms. =="
  putStrLn "Input/Output: *(+(1 2) +(-1 2)) -> True."
  print (prop_ParseInputEquivOutput "*(+(1 2) +(-1 2))")

  putStrLn "\n == Comments =="
  putStrLn "\n Parse seems to discard redundant closing parentheses. Automatic testing should generate forms where the number of opening/closing parentheses match."
  putStrLn "\n Whether this is within the specification of parse is unclear to us. Seems like an oversight/bug."
_main :: IO ()
_main = do
  exercise2