import Data.List
projectEuler24 :: [Int]
projectEuler24 = sort (permutations [0..9]) !! 999999


-- This is too computationally intensive to test.
-- We assume the Haskell functions are implemented correctly