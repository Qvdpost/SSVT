module Exercise1 where

import Data.Char
import Helper (exercise,tupleToList)
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

prop_Distribution :: (Integer, Integer, Integer, Integer) -> Integer -> Bool
prop_Distribution buckets n = all (\x -> fromIntegral (abs (x - fraction)) < error) (tupleToList buckets)
  where
    fraction = n `div` 4
    error = 0.05 * fromIntegral fraction

exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "Random Distribution"
  putStrLn $ "Each time a randomly generated number is generated, an appropriate bucket is "
  putStrLn $ "A bucket of size 4 keeps track of each boundry of randomly generated number"
  putStrLn $ "Each time a number is randomly generated, the appropriate index of a bucket is incremented."
  putStrLn $ "Example output:"
  nums <- probs 10000
  print (prop_Distribution (bucketize nums (0,0,0,0)) 10000)

_main :: IO ()
_main = do
  exercise1