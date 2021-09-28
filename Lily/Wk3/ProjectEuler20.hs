import Data.Char (digitToInt)

projectEuler20::Int
projectEuler20 = sumDigits (show factorial)

factorial :: Integer
factorial = product [1..100]

sumDigits::String->Int
sumDigits xs = foldr ((+) . digitToInt) 0 xs

-- I assume Haskell functions are implemented correctly.