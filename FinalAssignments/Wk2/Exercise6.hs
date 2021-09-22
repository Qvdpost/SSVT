module Exercise6 where

import Data.Char (chr, isLetter, isUpper,isLower, ord)
import Helper (exercise)
import Test.QuickCheck

-- rotate::Char->Int->Char
-- rotate c n = chr((ord c + n-97) `mod` 26+97)

{- 
  Specification of ROT13:
  Encrypts a string by using Caesar's Cypher algorithm using a key of 13.
  Each alphabetical character is replaced for an alphabetical character 13 places later (or cycled back to 'a') in the alphabet.
-}

rotate :: Char -> Int -> Char
rotate c n =  chr ((ord c + n - base) `mod` 26 + base)
  where
    base = if isUpper c then 65 else 97


rot13 :: String -> String
rot13 (c : cs)
  | isLetter c = rotate c 13 : rot13 cs
  | otherwise = c : rot13 cs
rot13 [] = ""

-- Two rotations should result in the original string
-- due to the English alphabet length (26 = 2 * 13)
prop_rot13Rotation :: String -> Bool
prop_rot13Rotation s = s == rot13 (rot13 s)

-- Encoding once should not return the same string as the input, iff there is an alphabetical letter in the string.
prop_rot13EncryptAny :: String -> Property
prop_rot13EncryptAny s = any (isLetter) s ==> s /= rot13 s

-- Encoding should result in the same string if there are no alphabetical letter in the string.
prop_rot13EncryptAll :: String -> Property
prop_rot13EncryptAll s = all (not . isLetter) s ==> s == rot13 s

-- No characters should get lost or be added during encryption.
prop_rot13Length :: String -> Bool
prop_rot13Length s = length s == length (rot13 s)

-- Every encryption should correctly be using a key of 13
propLettersRotateBy13 :: String -> Bool
propLettersRotateBy13 w = and $ zipWith (\ a b -> abs (ord a - ord b) == 13) (rot13 w) w


exercise6 :: IO ()
exercise6 = do
  putStrLn $ exercise 6 "Rot13"
  quickCheck prop_rot13EncryptAny
  quickCheck prop_rot13EncryptAll 
  print (prop_rot13Rotation "Some string.")
  print (propLettersRotateBy13 "Some string.")

_main :: IO ()
_main =
  exercise6