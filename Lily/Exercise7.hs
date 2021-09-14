-- Exercise 7 - Luhn's Exercise

-- 79927398713

luhn :: Integer -> Bool
luhn x = processDigits (reversal x) False `mod` 10 == 0

-- Provided in exercise 4 of worksheet
reversal :: Integer -> Integer
reversal = read . reverse . show

processDigits :: Integer -> Bool -> Integer
processDigits x isSecond
  | x == 0 = 0
  | isSecond = processDigit (x `mod` 10) + processDigits (x `div` 10) False
  | otherwise = (x `mod` 10) + processDigits (x `div` 10) True

isTwoDigits :: Integral a => a -> Bool
isTwoDigits x
  | x > 9 && x < 100 = True
  | otherwise = False

productDigit :: Integral a => a -> a
productDigit x = (x `mod` 10) + ((x `div` 10) `mod` 10)

processDigit :: Integral a => a -> a
processDigit x
  | isTwoDigits doubled = productDigit doubled
  | otherwise = doubled
  where
    doubled = x * 2

-- IIN information acquired from
-- https://www.groundlabs.com/blog/anatomy-of-a-credit-card-luhn-checks-bin-ranges-data-discovery/

-- prefix: 34|37
-- length: 15
isAmericanExpress :: Integer -> Bool
isAmericanExpress x
  | valid = (firstTwoDigits == 34 || firstTwoDigits == 37) && (len == 15)
  | otherwise = False
  where
    valid = luhn x
    len = length (show x)
    firstTwoDigits = read (take 2 (show x)) :: Int

-- prefix: 51-55
-- length: 16
isMaster :: Integer -> Bool
isMaster x
  | valid = (firstTwoDigits >= 51 || firstTwoDigits <= 55) && (len == 16)
  | otherwise = False
  where
    valid = luhn x
    len = length (show x)
    firstTwoDigits = read (take 2 (show x)) :: Int

-- prefix: 4
-- length: 13|16
isVisa :: Integer -> Bool
isVisa x
  | valid = (reversal x `mod` 10 == 4) && (len == 13 || len == 16)
  | otherwise = False
  where
    valid = luhn x
    len = length (show x)