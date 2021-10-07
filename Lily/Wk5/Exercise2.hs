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
import Test.QuickCheck ( (==>), Property, Gen,elements, generate, Arbitrary,arbitrary, suchThat, quickCheck, forAll,Positive,listOf)
import LTS (LTS,State,Label,LabeledTransition,createLTS,makeSet,tau)

-- Time Spent:

-- ltsGen :: Gen IOLTS
-- ltsGen = 

-- ([State], [Label], [Label], [LabeledTransition], State)


arbitraryLabeledTransition :: Gen LabeledTransition
arbitraryLabeledTransition = do
    i <- arbitrary ::Gen Integer
    j <- arbitrary ::Gen Integer
    randomString <- listOf $ elements ['a'..'z'] --"abcdefghijklmnopqrstuvwxyz"
    return (i,randomString,j)

arbitraryLTS ::Gen LTS
arbitraryLTS =  createLTS' (listOf (arbitrary ::Gen LabeledTransition))

genState:: Gen [LabeledTransition] -> Gen [State]
genState transitions =  do
    states <- makeSet (concatMap (\(from,_,to) -> [from, to]) transitions)

createLTS' :: Gen [LabeledTransition] -> Gen LTS
createLTS' transitions = do
    states <- makeSet (concatMap (\(from,_,to) -> [from, to]) transitions)
    return (states, filter (/= tau) $ makeSet (map (\(_,label,_) -> label) transitions), makeSet transitions, head states)


exercise2 :: IO ()
exercise2 = do
    putStrLn $ exercise 2 "XX"
    putStrLn "Example output: "

_main :: IO ()
_main = do
    exercise2