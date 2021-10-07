{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise2 where

import Data.List (intersect, nub, (\\), union, sort)
import Helper (exercise)
import Test.QuickCheck ( (==>), Property, Gen, generate, arbitrary, suchThat, quickCheck, forAll)

-- Time Spent: 

exercise2 :: IO ()
exercise2 = do
    putStrLn $ exercise 2 "XX"
    putStrLn "Example output: "

_main :: IO ()
_main = do
    exercise2