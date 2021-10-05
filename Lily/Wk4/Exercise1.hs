{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

-- Time Spent: 30 Minutes

module Exercise1 (_main, generateSetSystemRandom, generateSetQuickCheck) where

import Data.List (nub)
import Helper (exercise)
import SetOrd (Set(Set), list2set)
import System.Random (Random (randomR), getStdRandom)
import Test.QuickCheck (Arbitrary (arbitrary), generate, Gen, sample, listOf)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0, n))

randomFlip :: Num a => a -> IO a
randomFlip x = do
  b <- getRandomInt 1
  if b == 0 then return x else return (- x)

genIntList :: IO [Int]
genIntList = do
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
  x <- getRandomInt k
  y <- randomFlip x
  xs <- getIntL k (n -1)
  return (y : xs)

-- All above from lecture 2 notes
-- https://docs.google.com/presentation/d/1-2u4dnNMhIhaTHcrMoa08YWqrrx_cYdtVvlY9JpXKVI/edit#slide=id.g410331214a_0_10

getRandomListQuickCheck :: Gen [Int]
getRandomListQuickCheck = arbitrary :: Gen [Int]

generateSetQuickCheck :: Gen (Set Int)
generateSetQuickCheck = list2set <$> getRandomListQuickCheck

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = list2set . nub <$> listOf arbitrary

generateSetSystemRandom ::  IO (Set Int)
generateSetSystemRandom = list2set <$> genIntList


exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "Generate Random Set"
  putStrLn "Example output: "
  putStrLn "generateSetSystemRandom example: "
  x <-  generateSetSystemRandom
  putStrLn (show x)

  putStrLn ""
  
  putStrLn "generateSetQuickCheck sample:"
  sample generateSetQuickCheck
  

_main :: IO ()
_main = do
  exercise1