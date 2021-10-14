module MultiplicationTable where
import Test.QuickCheck
import Data.List

multiplicationTable :: Integer -> [Integer]
multiplicationTable x = map (*x) [1..10]

-- Property 1: Output list has exactly 10 elements
prop_tenElements :: [Integer] -> Integer -> Property
prop_tenElements o i = property $ length o == 10

-- Property 2: First number is the input
prop_firstElementIsInput :: [Integer] -> Integer -> Property
prop_firstElementIsInput o i = property $ head o == i

-- Property 3: The sum of the output is the input times the 10th triangle number
prop_sumIsTriangleNumberTimesInput :: [Integer] -> Integer -> Property
prop_sumIsTriangleNumberTimesInput o i = property $ sum o == sum [1..10] * i

-- Property 4: The difference between consecutive elements is the input
prop_linear :: [Integer] -> Integer -> Property
prop_linear o i = property $ linear o i

-- Property 5: Any element modulo the input is zero
prop_moduloIsZero :: [Integer] -> Integer -> Property
prop_moduloIsZero o i = i /= 0 ==> all (\v -> v `mod` i == 0) o

multiplicationTableProps :: [[Integer] -> Integer -> Property]
multiplicationTableProps = [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero]

linear :: [Integer] -> Integer -> Bool
linear [x] _ = True
linear (x:xs) y = head xs - x == y && linear xs y
