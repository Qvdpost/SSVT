{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}
module Exercise8 where

import Helper (exercise)
import Test.QuickCheck

import Data.List

data Boy = Matthew | Peter | Jack | Arnold | Carl
  deriving (Eq, Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Jack Matthew = True
accuses Carl Matthew = True
accuses Carl Jack = True
accuses _ _ = False

accusers :: Boy -> [Boy]
accusers a = [boy | boy <- boys, accuses boy a]

guilty, honest :: [Boy]
guilty = head (filter (statementTeacher . answers) (subsequences accused))
  where
    accused = [boy | boy <- boys, not (null (accusers boy))] -- Only boys who are accused can be guilty
honest = [boy | boy <- boys, answer boy guilty]

-- Just a copy of guilty that doesn't take the head of a list to remain type compliant.
allGuiltyBoys :: [[Boy]]
allGuiltyBoys = filter (statementTeacher . answers) (subsequences accused)
  where
    accused = [boy | boy <- boys, not (null (accusers boy))] -- Only boys who are accused can be guilty

statementTeacher :: [Bool] -> Bool
statementTeacher bs = length (filter (== True) bs) == 3

answer :: Boy -> [Boy] -> Bool
answer Matthew g = not (elem Carl g || elem Matthew g)
answer Peter g = elem Matthew g || elem Jack g
answer Jack g = not (answer Matthew g) && not (answer Peter g)
answer Arnold g = (answer Matthew g && not (answer Peter g)) || (not (answer Matthew g) && answer Peter g)
answer Carl g = (answer Matthew g && answer Peter g) || (not (answer Matthew g) && not (answer Peter g))

answers :: [Boy] -> [Bool]
answers g = [answer boy g | boy <- boys]

exercise8 :: IO ()
exercise8 = do
  putStrLn $ exercise 8 "Determine which boy is guilty and which boys made an honest statement."

  putStrLn "\n== The guilty boy =="
  print guilty

  putStrLn "\n== All guilty boys (Should be a list that contains only 1 list) =="
  print allGuiltyBoys

  putStrLn "\n== The honest boys (should be 3 elements long) =="
  print honest

_main :: IO ()
_main = do
  exercise8