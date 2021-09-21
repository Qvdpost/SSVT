module Lab2 where

import Data.Char
import Helper (exercise)


isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement (x:xs) (y:ys) | x /= y = isDerangement xs ys
                            | otherwise = False
isDerangement [] [] = True
isDerangement _ _ = False



exercise5 :: IO ()
exercise5 = do
  -- putStrLn $ exercise 1 "XXX"
    print ("Hi")

_main :: IO ()
_main = do
  exercise4