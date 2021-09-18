module Lab2 where

import Data.Char(ord,chr,isLetter)
import Helper (exercise)
import Test.QuickCheck ()

rotate::Char->Int->Char
rotate c n = chr((ord c + n-97) `mod` 26+97)

rot13::String->String
rot13 (c:cs)
  | isLetter c = rotate c 13 : rot13 cs
  | otherwise = c : rot13 cs
rot13 [] = ""

exercise5 :: IO ()
exercise5 =
  putStrLn $ exercise 5 "Rot13"

_main :: IO ()
_main =
  exercise5