
import Helper(exercise)

reversal :: Integer -> Integer
reversal = read . reverse . show

-- getPrimes :: [Integer]
-- getPrimes = [i | i <- [2 .. 10000], isPrime i && isPrime (reversal i)]

getPrimes :: [Integer]
getPrimes = [i | i <- [2 .. 10000], isPrime i]

isPrime :: Integral a => a -> Bool
isPrime x = null [i | i <- [2 .. ceiling (sqrt (fromIntegral x -1))], x `mod` i == 0]

-- fromIntegral needed, solution from here
-- https://stackoverflow.com/questions/6695267/get-sqrt-from-int-in-haskell

_main :: IO ()
_main = do
  exercise4