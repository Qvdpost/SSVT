
module Lab2 where

import Data.List
import Data.Maybe
import Data.Char
import System.Random
import Test.QuickCheck

-- | Helper functions

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- | Random number distribution: 90 mins.

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

divideInQuartiles x
    | x > 0 && x < 0.25 = 1
    | x < 0.5           = 2
    | x < 0.75          = 3
    | x < 1             = 4

distribution = map length . group . sort . map divideInQuartiles <$> probs 10000

-- | Recognizing triangles: 180 min.

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a == b && a == c                                         = Equilateral
    | a == b || b == c || a == c                               = Isosceles
    | a^2 + b^2 == c^2 || b^2 + c^2 == a^2 || a^2 + c^2 == b^2 = Rectangular
    | not (a + b > c && a + c > b && b + c > a)                = NoTriangle
    | otherwise                                                = Other

propEquilateral, propIsosceles :: (Integer, Integer, Integer) -> Bool
propEquilateral (a,b,c) = triangle a b c == Equilateral
propIsosceles   (a,b,c) = triangle a b c == Isosceles 

propRectangular, propNoTriangle, propOther :: (Integer, Integer, Integer) -> Bool
propRectangular (a,b,c) = triangle a b c == Rectangular
propNoTriangle  (a,b,c) = triangle a b c == NoTriangle
propOther       (a,b,c) = triangle a b c == Other

genEquilateral :: Gen (Integer, Integer, Integer)
genEquilateral = chooseInteger (1, 10^5) >>= \a -> return (a, a, a)

genIsosceles :: Gen (Integer, Integer, Integer)
genIsosceles = do
    a     <- chooseInteger (1, 10^5)
    b     <- chooseInteger (1, 10^5) `suchThat` (/= a)
    index <- choose (1, 3)

    return $ [(a, a, b), (a, b, a), (b, a, a)] !! index

genRectangular :: Gen (Integer, Integer, Integer)
genRectangular = do
    a <- chooseInteger (1, 10^5)
    b <- chooseInteger (1, 10^5)
    c <- chooseInteger (1, 10^5)
    -- c <- return $ sqrt (a^2 + b^2)  does not work?

    return (a, b, c) -- Need to shuffle.

genNoTriangle :: Gen (Integer, Integer, Integer)
genNoTriangle = do
    a <- chooseInteger (1, 10^5)
    b <- chooseInteger (1, 10^5)
    c <- chooseInteger (1, 10^6) `suchThat` (> a+b)

    return (a, b, c) -- Need to shuffle.

genOther :: Gen (Integer, Integer, Integer)
genOther = do
    a <- chooseInteger (1, 10^5)
    b <- chooseInteger (1, 10^5) `suchThat` (/= a)
    c <- chooseInteger (1, 10^5) `suchThat` (\c -> c /= b
                                                && a + b > c
                                                && a + c > b
                                                && b + c > a
                                                && a^2 + b^2 /= c^2
                                                && b^2 + c^2 /= a^2
                                                && a^2 + c^2 /= b^2)

    return (a, b, c) -- Need to shuffle.

-- | Testing properties strength: 75 min.

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- | a) Implement all properties from Exercise 3 from Workshop 2.
p1, p2, p3, p4 :: Int -> Bool
p1 x = even x && x > 3
p2 = even
p3 x = even x || x > 3
p4 x = (even x && x > 3) || even x


-- | b) Provide a descending strength list of all implemented properties.

compareStrength :: [a] -> (a -> Bool) -> (a -> Bool) -> Ordering
compareStrength domain x y
  | stronger domain x y = GT
  | otherwise           = LT

-- | This function has little value, since p1..p4 cannot be named,
sortedProperties = sortBy (compareStrength [(-10)..10]) [p1, p2, p3, p4]

{-
  Descending strength list of the properies from Exercise 3, Workshop 2 using domain [-10, 10].
    * (\x -> even x && x > 3)
    * even
    * (\x -> even x || x > 3)
    * (\x -> (even x && x > 3) || even x)

  To complete this exercise I use the compareStrength function
-}

-- | Recognizing permutations: 20 min
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = a /= b && length (concat (group a `intersect` group b)) == length a

-- | Derangement: 30 min.

isDerangement :: [Int] -> [Int] -> Bool
isDerangement a b = isPermutation a b && and (zipWith (/=) a b)

deran :: Int -> [[Int]]
deran n = filter (isDerangement [0..n-1]) (permutations [0..n-1])

propUniqueElementsUpToN :: [Int] -> Bool
propUniqueElementsUpToN l = sort l == [0..(length l - 1)]

-- | ROT13 encoding: 140 min.
--   ROT13 is a simple cipher that substitutes each letter in a given string with 13 places.

rotate :: Int -> Char -> Char
rotate n l
  | isLower l = cycle ['a'..'z'] !! (n + fromJust (l `elemIndex` ['a'..'z']))
  | isLetter l = cycle ['A'..'Z'] !! (n + fromJust (l `elemIndex` ['A'..'Z']))
  | otherwise = l

rot13 :: String -> String
rot13 = map (rotate 13)

propNonLettersRemain :: String -> Bool
propNonLettersRemain w = nonAlphaString == rot13 nonAlphaString
  where
    nonAlphaString = filter (not . isLetter) w

propLettersRotateBy13 :: String -> Bool
propLettersRotateBy13 w = and $ zipWith (\ a b -> abs (ord a - ord b) == 13) (rot13 w) w

genCharacters :: Gen String
genCharacters = vectorOf 30 $ elements [' '..'~']

genLetters :: Gen String
genLetters = vectorOf 30 $ elements (['a'..'z'] ++ ['A'..'Z'])

{-
  The following tests can be run:
    * quickCheck (forAll genLetters propLettersRotateBy13)
    * quickCheck (forAll genCharacters propNonLettersRemain)
-}

-- | IBAN Validation: 65 min.

convertToInt :: Char -> Int
convertToInt c
  | isUpper c = ord c - 65 + 10
  | otherwise = digitToInt c


ibanLength :: String -> Bool
ibanLength s = case countryLength of
  Just l -> l == length s
  Nothing -> False
  where
    countries :: [(String, Int)]
    countries = zip (words "AL AT BE BA BG HR CZ DO FO FR DE GR GT \
        \IS IL KZ LV LI LU MT MU MD NL PK PL RO SA SK ES CH TR GB \
        \AD AZ BH BR CR CY DK EE FI GE GI GL HU IE IT KW LB LT MK \
        \MR MC ME NO PS PT SM RS SI SE TN AE VG")
        [28,20,16,20,22,21,24,28,18,27,22,27,28,26,23,20,21,21,20,
        31,30,24,18,24,28,24,24,24,24,21,26,22,24,28,22,29,21,28,18,
        20,18,22,23,18,28,22,27,30,28,20,19,27,27,22,15,29,25,27,22,
        19,24,24,23,24]

    countryLength = lookup (take 2 s) countries


{-
  To test this function use a list of ibans you know are correct.
  To test whether the function works detects all cases when it 
  should mark an iban as invalid, generate valid numbers and add or remove
  one from the value or from the length. 
  Unfortunatly we didn't have enough time to implement this. 
-}
iban :: String -> Bool
iban s = length && integer `mod` 97 == 1
  where
    integer    = read (concatMap show digits)
    digits     = map convertToInt rearranged
    rearranged = drop 4 s ++ take 4 s
    length     = ibanLength s
