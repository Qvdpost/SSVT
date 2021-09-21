module Lab2 where

import Data.Char (chr, isLower, isUpper, ord)
import Helper (exercise)
import Test.QuickCheck ()

-- rotate::Char->Int->Char
-- rotate c n = chr((ord c + n-97) `mod` 26+97)

rotateUppercase :: Char -> Int -> Char
rotateUppercase c n = chr ((ord c + n -65) `mod` 26 + 65)

rotateLowercase :: Char -> Int -> Char
rotateLowercase c n = chr ((ord c + n -97) `mod` 26 + 97)

rot13 :: String -> String
rot13 (c : cs)
  | isUpper c = rotateUppercase c 13 : rot13 cs
  | isLower c = rotateLowercase c 13 : rot13 cs
  | otherwise = c : rot13 cs
rot13 [] = ""

-- Two rotations should result in the original string
-- due to the English alphabet length
test_rot13Rotation :: String -> Bool
test_rot13Rotation s = s == rot13 (rot13 s)

exercise5 :: IO ()
exercise5 =
  putStrLn $ exercise 6 "Rot13"

_main :: IO ()
_main =
  exercise5