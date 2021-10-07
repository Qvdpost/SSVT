{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}
-- Time Spent: 1 Hour 30 minutes
module Exercise5 where

import Data.List
import Helper (exercise)
import System.Random
import Test.QuickCheck

type Rel a = [(a, a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [(x, z) | (x, y) <- r, (w, z) <- s, y == w, x /= z] -- Added x /= z to prevent transitive relations to itself (when applied after symClos)

trClos :: Ord a => Rel a -> Rel a
trClos r | not $ null (transitives \\ r) = trClos (r ++ transitives)
         | otherwise = sort $ nub r
  where
    transitives = r @@ r

exercise5 :: IO ()
exercise5 = do
  putStrLn $ exercise 5 "Implement trClos :: Ord a => Rel a -> Rel a"
  putStrLn "Example output: (Input: [(1,2),(2,3),(3,4)])"
  print (trClos [(1,2),(2,3),(3,4)])

_main :: IO ()
_main =
  exercise5