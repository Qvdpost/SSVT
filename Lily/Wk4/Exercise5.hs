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
  nub [(x, z) | (x, y) <- r, (w, z) <- s, y == w]

-- 1 [(1,2),(2,3),(3,4)] -> [(1,2),(1,3),(1,4)]
match :: Eq a => a -> Rel a -> Bool -> Rel a
match m r@((x, y) : rs) begin
  | begin = (m, y) : match m rs begin
  | m == x = (m, y) : match m rs True
  | otherwise = match m rs False
match _ [] _ = []

trClos :: Ord a => Rel a -> Rel a
trClos r = [a | (x, _) <- r, a <- match x r False]

exercise5 :: IO ()
exercise5 = do
  putStrLn $ exercise 5 "Implement trClos :: Ord a => Rel a -> Rel a"
  putStrLn "Example output: "

_main :: IO ()
_main =
  exercise5