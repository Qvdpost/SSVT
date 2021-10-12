{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

-- Time Spent: 
module Exercise3 where

import Data.List
import Helper (exercise)
import System.Random
import Test.QuickCheck
import LTS
--Time Spent:

third :: (a, b, c) -> c
third (_,_,s) = s

step :: (Eq b, Eq a) => [(a, b, a)] -> a -> [(a, b, a)] -> [(a, b, a)]
step tr s xs
  | null trace = xs
  | null xs    = step tr s trace
  | otherwise   = step tr (third $ last xs) (xs++trace)
  where
      trace = filter (\(s',_,_) -> s'==s) tr

{-
  Using the step function, this function will find all reachable states,
  but I could not get it to work so it saves the path.
-}
reachableStates :: LTS -> [LabeledTransition]
reachableStates (_, _, transitions, start) = nub $ step transitions start []

{-
  This shows a list of all 'reachable' labels, but not the right strace function.
  I tried, but could not get it to work at all.
-}
straces :: LTS -> [Trace]
straces s = [map (\(_,l,_) -> l) (reachableStates s)]

exercise3 :: IO ()
exercise3 = do
  putStrLn $ exercise 3 "Implement: straces :: LTS -> [Trace]"


_main :: IO ()
_main =
  exercise3