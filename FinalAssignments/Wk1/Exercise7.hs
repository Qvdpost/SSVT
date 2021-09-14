{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}
module Exercise7 where

import Data.Char (digitToInt)
import Helper (exercise)
import Test.QuickCheck

reverseAndSplit :: Integer -> [Integer]
reverseAndSplit n = [toInteger (digitToInt n') | n' <- reverse (show n)]

evens :: [a] -> [a]
evens [] = []
evens (a : as) = a : odds as

odds :: [a] -> [a]
odds [] = []
odds (a : as) = evens as

-- Multiply numbers by 2 and if they're larger than 4 they will multiply to a number > 9 and should be manipulated accordingly (for Luhn's algo)
luhnDoubling :: Integer -> Integer
luhnDoubling n
  | n > 4 = (n * 2) `mod` 10 + 1
  | otherwise = n * 2

luhn :: Integer -> Bool
luhn n = sum (evens digits ++ doubles) `mod` 10 == 0
  where
    digits = reverseAndSplit n
    doubles = map luhnDoubling (odds digits)

-- First length is checked, then starting digits, then Luhn verifies the cc-number for typo's.
isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = length numbers == 15 && elem inn ["34", "37"] && luhn n
  where
    numbers = show n
    inn = take 2 numbers
isMaster n = length numbers == 16 && (inn >= 51 && inn <= 55) || (innNew >= 222100 && innNew <= 272099) && luhn n
  where
    numbers = show n
    inn = read (take 2 numbers)
    innNew = read (take 6 numbers)
isVisa n = elem (length numbers) [13, 16] && inn == 4 && luhn n
  where
    numbers = show n
    inn = read (take 1 numbers)

-- Valid (presumably) CC-numbers generated from cardguru.io
examplesVisa =
  [ 4556214512807378,
    4929564649169526,
    4485463550639492,
    4588215612507398,
    4929108167085902
  ]

examplesAmericanExpress =
  [ 342029816567644,
    349113280028730,
    378115933668527,
    341449182557097,
    343444129018814
  ]

examplesMastercard =
  [ 5279755250454487,
    5194217062353946,
    5364593647629453,
    5397838069245341,
    5522575429397143
  ]

exercise7 :: IO ()
exercise7 = do
  putStrLn $ exercise 7 "Verification functions to test for typos in credit card numbers of 3 popular businesses using Luhn's algorithm."

  putStrLn "\n== Test isVisa (True) =="
  putStrLn "Input/Output space coverage: Pre-generated CC-numbers."
  print (all (isVisa) examplesVisa)

  putStrLn "\n== Test isAmericanExpress (True) =="
  putStrLn "Input/Output space coverage: Pre-generated CC-numbers."
  print (all (isAmericanExpress) examplesAmericanExpress)

  putStrLn "\n== Test isMaster (True) =="
  putStrLn "Input/Output space coverage: Pre-generated CC-numbers."
  print (all (isMaster) examplesMastercard)

  putStrLn "\n== Test for non-Luhn cc-numbers (False) =="
  putStrLn "Input/Output space coverage: Manipulated pre-generated CC-numbers."
  putStrLn "\nVisa"
  print (all (isVisa) (map (+ 1) examplesVisa))

  putStrLn "\nAmerican Express"
  print (all (isAmericanExpress) (map (+ 1) examplesAmericanExpress))

  putStrLn "\nMastercard"
  print (all (isMaster) (map (+ 1) examplesMastercard))

  putStrLn "\n == Comments =="
  putStrLn "\nThese tests are not infallible, but they provide a check that for (assumed) valid CC-numbers the functions provide the correct answers."
  putStrLn "\nThen by invalidating Luhn (adding 1 throws off the sum mod 10) we show that the functions react accordingly and output False."
  putStrLn "\nUsing a mathematically proven luhn generator a more robust test could be written, but we don't have one of those unfortunately."

_main :: IO ()
_main = do
  exercise7