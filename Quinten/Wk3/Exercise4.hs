{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise4 where

import Data.List
import Helper (exercise)
import Lecture3 ( Form(..))
import System.Random
import Test.QuickCheck

randomProp::IO Form
randomProp = do
  prop_num <- randomRIO (1, 3)
  return (Prop prop_num)

getManyRandomProps::Int -> [IO Form]
getManyRandomProps n
  | n > 0 = randomProp : getManyRandomProps (n-1)
  | n == 0 = []
  | otherwise = error "Not happy 😥"

randomNegate::IO Form
randomNegate = do
  Neg <$> randomProp

randomConjunction::IO Form
randomConjunction = do
  p <-randomProp
  q <-randomProp
  return (Cnj (p:[q]))

randomDisjunction::IO Form
randomDisjunction = do
  p <-randomProp
  q <-randomProp
  return (Dsj (p:[q]))

randomForm::IO Form
randomForm = do
  n <-randomRIO (1::Int, 4)
  case n of
    1 -> randomConjunction
    2 -> randomDisjunction
    3 -> randomNegate
    4 -> randomProp

-- Below does not compile
-- getManyRandomForms::[IO Form]
-- getManyRandomForms = do
--   n <- randomRIO (1,4)
--   return [ randomForm | randomForm <- [0..n]]


exercise4 :: IO ()
exercise4 = do
  putStrLn $ exercise 4 "Using Random Testing Methods test previous Exercise"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise4