module Lab2 where

import Data.Char
import Helper (exercise)
import System.Random
import Data.Foldable

-- Time spent, 30 minutes + ~8 cleanup
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n -1)
  return (p : ps)

bucketize :: [Float] -> (Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer)
bucketize (x:xs) (a,b,c,d) | x <= 0.25 = bucketize xs (a+1,b,c,d)
                           | x <= 0.50 = bucketize xs (a,b+1,c,d)
                           | x <= 0.75 = bucketize xs (a,b,c+1,d)
                           | otherwise = bucketize xs (a,b,c,d+1)
bucketize [] buckets = buckets

tupleToList :: (Integer, Integer, Integer, Integer) -> [Integer]
tupleToList (a,b,c,d) = [a,b,c,d]

prop_Distribution :: (Integer, Integer, Integer, Integer) -> Integer -> Bool
prop_Distribution buckets n = all (\x -> abs (x - fraction) < error) (tupleToList buckets)
  where
    fraction = (fromIntegral n :: Float) / 4.0
    error = 0.05 * fraction


exercise1 :: IO ()
exercise1 = do
  -- putStrLn $ exercise 1 "XXX"
    nums <- probs 10000
    print (prop_Distribution (bucketize nums (0,0,0,0)) 10000)

_main :: IO ()
_main = do
  exercise1