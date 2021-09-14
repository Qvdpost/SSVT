module ExpectationsExample where
import Test.QuickCheck

-- Awfully verbose recursive solution
multiplicationTableRecursive :: Integer -> [Integer]
multiplicationTableRecursive x = multiplicationTableRecursive' x 1

multiplicationTableRecursive' :: Integer -> Integer -> [Integer]
multiplicationTableRecursive' x 10 = [x * 10]
multiplicationTableRecursive' x y = (x * y) : multiplicationTableRecursive' x (y + 1)


-- == Assignment 1: Calculate the multiplication table ==

-- Solution 1: Multiplication table using iterate. We didn't choose it as the final solution since it's slightly more verbose than multiplicationTable2.
multiplicationTable1 :: Integer -> [Integer]
multiplicationTable1 x = take 10 $ iterate (+x) x
-- (a -> a) -> a -> [a]

-- Solution 2: Multiplication table using map. Simple and concise :)
multiplicationTable2 :: Integer -> [Integer]
multiplicationTable2 x = map (*x) [1..10]

multiplicationTable2 :: Integer -> [Integer]
multiplicationTable2 = map (*x) [1..10]

multiplicationTable2' :: Integer -> [Integer]
multiplicationTable2' x = [n*x | n <- [1..10]]

-- Solution 3: Multiplication table using list syntax. Using QuickCheck, we found a bug when calling `multiplicationTable3 0`, it would result in an infinite list.
multiplicationTable3 :: Integer -> [Integer]
multiplicationTable3 x = [x, x*2..x*10]

multiplicationTable3' :: Integer -> [Integer]
multiplicationTable3' x = take 10 [x, x*2..]

-- Property 1: Output list has exactly 10 elements
prop_tenElements :: (Integer -> [Integer]) -> Integer -> Bool
prop_tenElements f x = length (f x) == 10

-- Property 2: First number is the input
prop_firstElementIsInput :: (Integer -> [Integer]) -> Integer -> Bool
prop_firstElementIsInput f x = head (f x) == x

-- Property 3: The sum of the output is the input times the 10th triangle number
prop_sumIsTriangleNumberTimesInput :: (Integer -> [Integer]) -> Integer -> Bool
prop_sumIsTriangleNumberTimesInput f x = sum (f x) == sum [1..10] * x

-- Property 4: The difference between consecutive elements is the input
prop_linear :: (Integer -> [Integer]) -> Integer -> Bool
prop_linear f x = linear (f x) x

-- Property 5: Any element modulo the input is zero
prop_moduloIsZero :: (Integer -> [Integer]) -> Integer -> Property
prop_moduloIsZero f x = x /= 0 ==> all (\v -> v `mod` x == 0) (f x)

linear :: [Integer] -> Integer -> Bool
linear [x] _ = True
linear (x:xs) y = head xs - x == y && linear xs y

exercise :: Integer -> String -> String
exercise x name | x == 1    = banner
                  | otherwise = "\n" ++ banner
   where contents = "== Exercise " ++ show x ++ ": " ++ name ++ " =="
         delimiter = replicate (length contents) '='
         banner = delimiter ++ "\n" ++ contents ++ "\n" ++ delimiter

-- In my main I made a selection of the 'best' implementation, and wrote some documentation surrounding it. This is just to illustrate a potential solution, not the best one.
exercise1 :: IO()
exercise1 = do
  putStrLn $ exercise 1 "Calculate the multiplication table"
  putStrLn "Chosen solution: Create list from 1 to 10, then multiply each value by the input number. Two more discarded solutions and rationale are provided in our assignment file."
  putStrLn $ "Example output: " ++ show (multiplicationTable2 10)

  putStrLn "\n<Property 1: The output list contains exactly ten numbers />"
  quickCheck $ prop_tenElements multiplicationTable2
  putStrLn "Input/Output space coverage: This property covers the entire input/output space."
  putStrLn "Conclusion: We only check that the length matches specification. The elements in the list are not checked, which leaves a large margin for error."

  putStrLn "\n<Property 2: First number is the input />"
  quickCheck $ prop_firstElementIsInput multiplicationTable2
  putStrLn "Input/Output space coverage: This property covers the entire input/output space."
  putStrLn "Conclusion: We only check that the first element matches specification. The rest of the elements in the list are not checked."

  putStrLn "\n<Property 3: The sum of the output is the input times the 10th triangle number />"
  quickCheck $ prop_sumIsTriangleNumberTimesInput multiplicationTable2
  putStrLn "Input/Output space coverage: This property covers the entire input/output space."
  putStrLn "Conclusion: We check that the sum of the output is correct. This is a derived value that gives an indication of the data inside, but it doesn't say anything about the order and the exact values in the list."

  putStrLn "\n<Property 4: The output list increments linearly in increments equal to the input />"
  quickCheck $ prop_linear multiplicationTable2
  putStrLn "Input/Output space coverage: This property covers the entire input/output space."
  putStrLn "Conclusion: It checks all the numbers in the entire list, but doesn't check whether the first value is correct and whether the list has the correct size."

  putStrLn "\n<Property 5: Any element modulo the input is zero />"
  quickCheck $ prop_moduloIsZero multiplicationTable2
  putStrLn "Input/Output space coverage: This property covers the entire input/output space, except for zero. For one and minus one the property doesn't have any effect (as in, it doesn't eliminate any incorrect output)."
  putStrLn "Conclusion: It checks the entire list, but performs a rather weak assertion."

  putStrLn "\nConclusion:"
  putStrLn "If we combine properties (1, 3, 4) there are no counterexamples that can refute the validity (only 3 and 4 doesn't work for zero). Property 4 could potentially be split into two 'isLinear' and 'firstDeltaIsInput' properties (such that they are more atomic), that cover a subset of the cases. Property 2 is redundant over the combination of property 3 and 4. Although, all properties are still valid to get a more fine-grained view on the validity of the function under test."

main :: IO()
main = do
  exercise1

  putStrLn "That's all folks! *imagine loony tunes theme playing*"
