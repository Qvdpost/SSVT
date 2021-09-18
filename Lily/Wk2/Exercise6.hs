module Lab2 where

-- import Data.Char(ord,chr,isLetter)

import Data.Char (isLetter, ord)
import Data.Map (Map, fromList, lookup)
import Helper (exercise)
import Test.QuickCheck

iban :: String -> Bool
iban cs
  | validCountry = (read (lettersToDigits (drop 4 cs ++ initialChars)) :: Integer) `mod` 97 == 1
  | otherwise = False
  where
    validCountry = validateCountry (take 2 cs) (length cs)
    initialChars = take 4 cs

validateCountry :: String -> Int -> Bool
validateCountry xs len = case Data.Map.lookup xs getCountryMap of
  Just n -> n == len
  nothing -> False

lettersToDigits :: String -> String
lettersToDigits (c : cs)
  | isLetter c = show (ord c - 55) ++ lettersToDigits cs
  | otherwise = c : lettersToDigits cs
lettersToDigits [] = ""

-- Information taken from here
-- https://www.iban.com/structure

-- Converting to map from here
-- https://stackoverflow.com/questions/50304525/haskell-create-key-value-map

getCountryMap :: Map String Int
getCountryMap =
  fromList
    ( zip
        (words "AL AD AT AZ BH BY BE BA BR BG CR HR CY CZ DK DO EG SV EE FO FI FR GE DE GI GR GL GT VA HU IS IQ IE IL IT JO KZ XK KW LV LB LY LI LT LU MT MR MU MD MC ME NL MK NO PK PS PL PT QA RO LC SM ST SA RS SC SK SI ES SD SE CH TL TN TR UA AE GB VG")
        ( map
            (\x -> read x :: Int)
            (words "28 24 20 28 22 28 16 20 29 22 22 21 28 24 18 28 29 28 20 18 18 27 22 22 23 27 18 28 22 28 26 23 22 23 27 30 20 20 30 21 28 25 21 20 20 31 27 30 24 27 22 18 19 15 24 29 28 25 29 24 32 27 25 24 22 31 24 19 24 18 24 21 23 24 26 29 23 22 24")
        )
    )

exercise6 :: IO ()
exercise6 =
  putStrLn $ exercise 6 "IBAN Validator"

_main :: IO ()
_main =
  exercise6