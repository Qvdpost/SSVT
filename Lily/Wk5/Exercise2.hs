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
import Test.QuickCheck ( (==>), Property, Gen, generate, Arbitrary,arbitrary, suchThat, quickCheck, forAll,Positive,listOf)
import LTS (LTS,State,Label,LabeledTransition,createLTS)

-- Time Spent:

-- ltsGen :: Gen IOLTS
-- ltsGen = 

-- ([State], [Label], [Label], [LabeledTransition], State)


arbitraryLabeledTransition :: Arbitrary a => LabeledTransition
arbitraryLabeledTransition = do
    i <- arbitrary ::Positive
    j <- arbitrary ::Positive
    randomString <- listOf $ elements "abcdefghijklmnopqrstuvwxyz"
    return (i,randomString,j)

-- arbitraryIOLTS :: Arbitrary a => Gen ([State], [Label], [Label], [LabeledTransition], State)
-- arbitraryIOLTS = do
--     i <- arbitrary ::Int
--     return (createLTS )


exercise2 :: IO ()
exercise2 = do
    putStrLn $ exercise 2 "XX"
    putStrLn "Example output: "

_main :: IO ()
_main = do
    exercise2