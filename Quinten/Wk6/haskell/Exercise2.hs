{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}
{-# LANGUAGE ParallelListComp #-}
module Exercise2 where

import Prelude hiding ((<>))

import Helper (exercise)
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Exercise1
import Data.List
import Text.PrettyPrint.Boxes
import Control.Monad

allMutators :: [[Integer] -> Gen [Integer]]
allMutators = [addElements, removeElements, repeatMutator, subsectionMutator, chainMutator]

mutatorNames = ["addElements", "removeElements", "repeatMutator", "subsectionMutator", "chainMutator"]


allProps :: [[Integer] -> Integer -> Bool]
allProps = [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_moduloIsZero, prop_linear]

propNames = ["prop_tenElements", "prop_firstElementIsInput", "prop_sumIsTriangleNumberTimesInput", "prop_moduloIsZero", "prop_linear"]


-- Time Spent:
mutate' :: ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> Integer -> Gen Bool
mutate' prop fut mutator input = propertyExecutor prop (mutator $ fut input) input

countSurvivors :: Int -> ([Integer] -> Gen [Integer]) -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> IO Int
countSurvivors mutants mutator prop fut = tests >>= \x -> return $ length $ filter (== True) x
  where
    tests = generate $ vectorOf mutants (mutate' prop fut mutator 3)

testAllMutators :: Int -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> IO [Int]
testAllMutators mutants prop fut = sequence [countSurvivors mutants mutator prop fut | mutator <- allMutators]

testAllProps :: Int -> (Integer -> [Integer]) -> IO [[Int]]
testAllProps mutants fut = sequence [testAllMutators mutants prop fut | prop <- allProps]


-- https://codereview.stackexchange.com/questions/171992/pretty-printed-tables-in-haskell
pad width x = x ++ replicate k ' '
  where k = width - length x

fmt_column :: [String] -> Box
fmt_column items = hsep // vcat left (intersperse hsep (map (text.pad width) items)) // hsep
  where width = maximum $ map length items
        hsep = text ( replicate width '-' )

table :: [[String]] -> Box
table rows = vsep <> hcat top (intersperse vsep (map fmt_column columns)) <> vsep
  where
    columns = transpose rows
    nrows = length rows
    vsep =  vcat left $ map char ("+" ++ (concat $ replicate nrows "|+"))

exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "Implement ltsGen :: Gen IOLTS && for a non-io LTS"

  putStrLn "Example Output: addElements prop_firstElementIsInput multiplicationTable"
  survivors <- countSurvivors 4000 addElements prop_firstElementIsInput multiplicationTable
  print survivors

  putStrLn "\nExample Output allMutators: [addElements, removeElements]"
  checks <- testAllMutators 4000 (head allProps) multiplicationTable
  print checks

  putStrLn "\nExample Output allProps: [prop_tenElements, prop_firstElementIsInput...]"
  tests <- testAllProps 4000 multiplicationTable
  putStrLn $ render $ table $ ("Survivors: " : mutatorNames) : [propName : [show val | val <- test] | test <- tests | propName <- propNames]


  putStrLn "\n"


_main :: IO ()
_main = do
  exercise2