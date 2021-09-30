{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

-- Time Spent: 30 Minutes

module Exercise1 where

import Data.List(nub)
import Helper (exercise)
import System.Random ( getStdRandom, Random(randomR) )
import Test.QuickCheck ( generate, Arbitrary(arbitrary) )
import SetOrd(Set,list2set)

getRandomInt::Int->IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Num a => a -> IO a
randomFlip x = do
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

-- All above from lecture 2 notes
-- https://docs.google.com/presentation/d/1-2u4dnNMhIhaTHcrMoa08YWqrrx_cYdtVvlY9JpXKVI/edit#slide=id.g410331214a_0_10

getRandomListQuickCheck::IO [Int]
getRandomListQuickCheck = generate arbitrary :: IO [Int]

generateSetQuickCheck::IO Set
generateSetQuickCheck = do
    list2set . nub <$> getRandomListQuickCheck

generateSetSystemRandom::IO Set a
generateSetSystemRandom = do
    list2set . nub <$> genIntList

exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "Generate Random Set"
  putStrLn "Example output: "
  putStrLn $ "generateSetQuickCheck" show generateSetSystemRandom
  putStrLn $ "generateSetQuickCheck" show generateSetQuickCheck

_main :: IO ()
_main = do
  exercise1