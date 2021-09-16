module Lab2 where

import Data.Char
import Helper (exercise)
import System.Random

-- Time spent, 30 minutes + ~8 cleanup
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n -1)
  return (p : ps)

sortIO::IO ()
sortIO = do
    nums <- probs 1000000
    putStrLn "(.0->.25)"
    -- print $ length [i | i<- nums, i < 0.26]
    print $ length (filter (< 0.26) nums)
    putStrLn "(.26->.50)"
    -- print $ length [i | i<- nums, i > 0.25 && i <= 0.5]
    print $ length (filter (\ x -> x > 0.25 && x <= 0.5) nums)

    putStrLn "(.50->.75)"
    -- print $ length [i | i<- nums, i > 0.5 && i <= 0.75]
    print $ length (filter (\ x -> x > 0.5 && x <= 0.75) nums)

    putStrLn "(.75->1.0)"
    -- print $ length [i | i<- nums, i > 0.75]
    print $ length (filter (> 0.75) nums)


exercise1 :: IO ()
exercise1 = do
  -- putStrLn $ exercise 1 "XXX"
  sortIO

_main :: IO ()
_main = do
  exercise1