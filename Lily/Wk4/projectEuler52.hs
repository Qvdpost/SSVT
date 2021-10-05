-- https://projecteuler.net/problem=52
-- Time: 35 minutes
import Data.List ((\\))

transitive :: String -> String -> Bool
transitive s1 s2 = null (s1 \\ s2)

-- checkDigit::Integer->Bool
-- checkDigit x = and [True | i<-[2..6] | transitive (show (i*x)) (show x)]
-- Above produces a odd Haskell Language Server suggesting. ^
--                          This i is to be replaced with i ^
-- Ensure this is at the top of the haskell file: {-# LANGUAGE ParallelListComp #-}
-- It makes sense that i is not in scope, the suggestion is odd

checkDigit :: Integer -> Bool
checkDigit x = and [transitive (show (i * x)) (show x) | i <- [2 .. 6]]

-- projectEuler52 :: [Integer]
projectEuler52 :: Integer
projectEuler52 = head (filter checkDigit [1 ..])