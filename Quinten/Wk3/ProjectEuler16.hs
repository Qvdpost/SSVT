import Data.Char (digitToInt)

projectEuler16::Int
projectEuler16 = process (show (2^1000))

process::String->Int
process xs = foldr ((+) . digitToInt) 0 xs

-- Original
-- process::String->Int
-- process(x:xs) = (digitToInt x) + (process xs)
-- process [] = 0

-- I assume Haskell functions are implemented correctly.