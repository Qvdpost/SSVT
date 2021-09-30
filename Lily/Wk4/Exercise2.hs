{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise2 where

import Data.List ( intersect, nub )
import Helper (exercise)
import System.Random ()
import Test.QuickCheck
import SetOrd(Set)

setInters a b = nub $ intersect a b


setIntersection::Set a -> Set a -> Set a -- This will need to be: setIntersection::Set a -> Set b -> Set c
setIntersection a b = nub $ intersect (a) (b)

exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "Testing Lecture Parse Function"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise2