
module Lab1 where
import Data.List
import Test.QuickCheck
import Debug.Trace
import Data.Char (digitToInt)

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


-------------------Exercise 1---------------------------------

powerSum :: Integer -> Integer -> Integer
powerSum m n = sum $ map (^m) [1..n]

quadraSum :: Integer -> Integer
quadraSum n = (n * (n + 1) * (2 * n + 1)) `div` 6

thirdSum :: Integer -> Integer
thirdSum n = (n * (n + 1) `div` 2)^2

testQuadraSum :: Integer -> Property
testQuadraSum n = n >= 0 ==> powerSum 2 n == quadraSum n

testBaseQuadraSum :: Bool
testBaseQuadraSum = quadraSum 1 == 1

testThirdSum :: Integer -> Property
testThirdSum n = n >= 0 ==> powerSum 3 n == thirdSum n

testBaseThirdSum :: Bool
testBaseThirdSum = thirdSum 1 == 1

exercise :: Integer -> String -> String
exercise x name | x == 1    = banner
                  | otherwise = "\n" ++ banner
   where contents = "== Exercise " ++ show x ++ ": " ++ name ++ " =="
         delimiter = replicate (length contents) '='
         banner = delimiter ++ "\n" ++ contents ++ "\n" ++ delimiter

exercise1 :: IO ()
exercise1 = do
  putStrLn $ exercise 1 "Calculate sum of a number-sequence for every number to the power of m"

  putStrLn $ "Example output: " ++ show (quadraSum 10)

  putStrLn "\n== Base case test of n = 1 for the quadratic sum =="
  putStrLn "Input/Output space coverage: Only number 1 as input is covered."
  print testBaseQuadraSum

  putStrLn "\n== Proof induction for quadratic sum (Success) =="
  putStrLn "Input/Output space coverage: Only natural numbers as input are covered."
  quickCheck testQuadraSum


  putStrLn "\n== Base case test of n = 1 for the power of three sum =="
  putStrLn "Input/Output space coverage: Only number 1 as input is covered."
  print testBaseThirdSum

  putStrLn "\n== Proof induction for power of three sum (Success) =="
  putStrLn "Input/Output space coverage: Only natural numbers as input are covered."
  quickCheck testThirdSum

-------------------Exercise 2---------------------------------

testExactSubsetLength :: Integer -> Property
testExactSubsetLength n = n >= 0  && n < 25 ==> count == length (take count (subsequences as))
  where
    as = [1..n]
    count = 2^length as

testPlusOneSubsetLength :: Integer -> Property
testPlusOneSubsetLength n = n >= 0  && n < 25 ==> count == length (take count (subsequences as))
  where
    as = [1..n]
    count = 2^length as + 1

exercise2 :: IO ()
exercise2 = do
  putStrLn $ exercise 2 "Test that a list with length n has 2^n subsets."

  putStrLn "\n== Check length of subsets is 2^length of list (Success) =="
  putStrLn "Input/Output space coverage: Only feasible (<25) natural numbers as input are covered."
  quickCheck testExactSubsetLength

  putStrLn "\n== Check that the length of subsets can never reach 2^length of list + 1 (Fail) =="
  putStrLn "Input/Output space coverage: Only natural numbers as input are covered."
  quickCheck testPlusOneSubsetLength

  putStrLn "\n == Comments =="
  putStrLn "\nThe complexity of testing that the number of subsequences for a sequence of length n is 2^n."
  putStrLn "\nEvery increment to n doubles the amount of work previously required to do. Leading to rapidly increasing runtimes for tests with n larger than the arbitrary number 25."
  putStrLn "\n"
  putStrLn "\nRight now we're testing wether subsequences produces a list as long as it's supposed to. Not wether the elements inside actually represent the powerset."
  putStrLn "\nBesides, the tests only cover a tiny part of the entire domain, testing the entire domain is not feasible. Even testing a small part of the domain is not feasible (domain being all positive numbers)."




-------------------Exercise 3---------------------------------

-- We can use Int here because the test is preconditioned to run for inputs between 0 and 10 and '10!' still fits neatly within an Int
factorial :: Int -> Int
factorial n | n < 0 = error "negative factorial"
            | n == 0 = 1
            | otherwise = n * factorial (n-1)

testPermutations :: Int -> Property
testPermutations n = n >= 0 && n < 10 ==> length (permutations as) == count
  where
    as = [1..n]
    count = factorial n

exercise3 :: IO ()
exercise3 = do
  putStrLn $ exercise 3 "Test that the closed formula of permutations of a list equals length factorial of that list."

  putStrLn "\n== Check length of permutations is length! of list (Success) =="
  putStrLn "Input/Output space coverage: Only feasible (<10) natural numbers as input are covered."
  quickCheck testPermutations

  putStrLn "\n == Comments =="
  putStrLn "\nThe complexity of testing that the number of permutations for a sequence of length n is n!."
  putStrLn "\nThis problem scales horribly and quickly becomes way too large to test thoroughly."
  putStrLn "\n"
  putStrLn "\nRight now we're testing wether permutations produces a list as long as it's supposed to. Not wether the elements inside actually represent all the permutations."
  putStrLn "\nBesides, the tests only cover a miniscule part of the entire domain."

-------------------Exercise 4---------------------------------
reversal :: Integer -> Integer
reversal = read . reverse . show

revPrimes :: Integer -> [Integer]
revPrimes n = filter (prime . reversal) (takeWhile (<n) primes)

exercise4 :: IO ()
exercise4 = do
  putStrLn $ exercise 4 "Function to find all primes whose reversals are also primes."

  print (revPrimes 10000)

  putStrLn "\n == Comments =="
  putStrLn "\nTesting this function could be done in a number of ways, but most would revolve around using a different approach to find all of the same prime numbers."
  putStrLn "\nDifferent checks would consist of verifying each number is indeed a prime number, and thus be a test of the 'prime' function that was used."
  putStrLn "\nEach of the property tests would not be testing an atom so to say, but rather a function that should have its own tests as well."
  putStrLn "\nThis function assumes that the reversal of a number is allowed to be the same as the number itself."

-------------------Exercise 5---------------------------------
-- Just a sum of an arbitrary number of primes. Specifically chosen for this exercise.
sumBunch = sum (take 101 primes)

-- This function should be provided with the sum of the first 101 primes, so that it can 
-- dynamically update the sum, instead of re-calculating each recursive step.
primes101 :: [Integer] -> Integer -> Integer
primes101 (x:xs) n | prime n = n
                   | otherwise = primes101 nextPrimes newSum
  where
    newPrime = xs !! 100 -- Since the head is already removed from xs; index 100 is the followup prime.
    nextPrimes = xs ++ [newPrime]
    newSum = n - x + newPrime


exercise5 :: IO ()
exercise5 = do
  putStrLn $ exercise 5 "Function to find the smallest prime that is a sum of 101 consecutive primes."

  print (primes101 primes sumBunch)

  putStrLn "\n == Comments =="
  putStrLn "\nTesting the correctness of the function would be writing the inverse of the function itself."
  putStrLn "\nThe output would be taken and then you'd look for a smaller prime than than number and see if the preceding 101 prime numbers sums up to that target."
  putStrLn "\nSince the primes function is already supposed to do that, testing this property redundant."
  putStrLn "\nYou could however test wether a similar function with an alternative implementation gives the same result. Then you'd be cover some possible programming errors, though it would probably best to not use the same helper functions in that case."
  putStrLn "\nThat function would need to be tested too though. For which you would need another function, and you would go down a recursive test rabbit hole."


-------------------Exercise 6---------------------------------
conjecture :: [Integer] -> Bool
conjecture [] = error "Empty sum is not covered in the conjecture."
conjecture ns = prime (product ns + 1)

testConjecture :: Int -> Property
testConjecture n = n > 0 ==> conjecture (take n primes)

-- This function provides a infinitely increasing list of counterexamples for the conjecture, starting with the smallest example.
testConjectureLinearInf :: Int -> Int -> [[Integer]]
testConjectureLinearInf n m | n > 0 = if not (conjecture newPrimes) then [newPrimes] ++ testConjectureLinearInf (n-1) (m+1) else testConjectureLinearInf n (m+1)
                            | otherwise = []
                          where
                            newPrimes = take m primes


exercise6 :: IO ()
exercise6 = do
  putStrLn $ exercise 6 "Disproves a conjecture by providing counterexamples that fail to hold true."

  putStrLn "\n== Test conjecture (Fail) =="
  putStrLn "Input/Output space coverage: Natural numbers greater than 1 are considered in the conjecture and in the test."
  quickCheck testConjecture

  putStrLn "\n== Provides a list of counterexamples sorted from smallest to largest  =="
  putStrLn "Input/Output space coverage: A list of length n can be specified and counterexamples starting with size m can be specified."
  print (testConjectureLinearInf 3 1)

  putStrLn "\n == Comments =="
  putStrLn "\nQuickTesting this conjecture has an obvious shortcoming in the fact that it could (depending on the random number generator) generate 100 test cases of which none are an actual counterexample."
  putStrLn "\nLinearly going through each possible counterexample eliminates that possibility, but comes at the disadvantage of not reaching certain test cases until very late."

-------------------Exercise 7---------------------------------
reverseAndSplit :: Integer -> [Integer]
reverseAndSplit n = [toInteger (digitToInt n') | n' <- reverse (show n)]

evens :: [a] -> [a]
evens [] = []
evens (a:as) = a : odds as

odds :: [a] -> [a]
odds [] = []
odds (a:as) = evens as

-- Multiply numbers by 2 and if they're larger than 4 they will multiply to a number > 9 and should be manipulated accordingly (for Luhn's algo)
luhnDoubling :: Integer -> Integer
luhnDoubling n | n > 4 = (n * 2) `mod` 10 + 1
               | otherwise = n * 2

luhn :: Integer -> Bool
luhn n = sum (evens digits ++ doubles) `mod` 10 == 0
  where
    digits = reverseAndSplit n
    doubles = map luhnDoubling (odds digits)

-- First length is checked, then starting digits, then Luhn verifies the cc-number for typo's.
isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = length numbers == 15 && elem inn ["34", "37"] && luhn n
  where
    numbers = show n
    inn = take 2 numbers
isMaster n = length numbers == 16 && (inn >= 51 && inn <= 55) || (innNew >= 222100 && innNew <= 272099) && luhn n
  where
    numbers = show n
    inn = read (take 2 numbers)
    innNew = read (take 6 numbers)
isVisa n = elem (length numbers) [13, 16] && inn == 4 && luhn n
  where
    numbers = show n
    inn = read (take 1 numbers)

-- Valid (presumably) CC-numbers generated from cardguru.io
examplesVisa = [
  4556214512807378,
  4929564649169526,
  4485463550639492,
  4588215612507398,
  4929108167085902
 ]

examplesAmericanExpress = [
  342029816567644,
  349113280028730,
  378115933668527,
  341449182557097,
  343444129018814
 ]

examplesMastercard = [
  5279755250454487,
  5194217062353946,
  5364593647629453,
  5397838069245341,
  5522575429397143
 ]

exercise7 :: IO ()
exercise7 = do
  putStrLn $ exercise 7 "Verification functions to test for typos in credit card numbers of 3 popular businesses using Luhn's algorithm."

  putStrLn "\n== Test isVisa (True) =="
  putStrLn "Input/Output space coverage: Pre-generated CC-numbers."
  print (all (isVisa) examplesVisa)

  putStrLn "\n== Test isAmericanExpress (True) =="
  putStrLn "Input/Output space coverage: Pre-generated CC-numbers."
  print (all (isAmericanExpress) examplesAmericanExpress)

  putStrLn "\n== Test isMaster (True) =="
  putStrLn "Input/Output space coverage: Pre-generated CC-numbers."
  print (all (isMaster) examplesMastercard)

  putStrLn "\n== Test for non-Luhn cc-numbers (False) =="
  putStrLn "Input/Output space coverage: Manipulated pre-generated CC-numbers."
  putStrLn "\nVisa"
  print (all (isVisa) (map (+1) examplesVisa))

  putStrLn "\nAmerican Express"
  print (all (isAmericanExpress) (map (+1) examplesAmericanExpress))

  putStrLn "\nMastercard"
  print (all (isMaster) (map (+1) examplesMastercard))

  putStrLn "\n == Comments =="
  putStrLn "\nThese tests are not infallible, but they provide a check that for (assumed) valid CC-numbers the functions provide the correct answers."
  putStrLn "\nThen by invalidating Luhn (adding 1 throws off the sum mod 10) we show that the functions react accordingly and output False."
  putStrLn "\nUsing a mathematically proven luhn generator a more robust test could be written, but we don't have one of those unfortunately."





-------------------Exercise 8---------------------------------
data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Jack Matthew = True
accuses Carl Matthew = True
accuses Carl Jack = True
accuses _ _ = False

accusers :: Boy -> [Boy]
accusers a = [boy | boy <- boys, accuses boy a]

guilty, honest :: [Boy]
guilty = head (filter (statementTeacher . answers) (subsequences accused))
  where
    accused = [boy | boy <- boys, not (null (accusers boy))] -- Only boys who are accused can be guilty
honest = [boy | boy <- boys, answer boy guilty]

-- Just a copy of guilty that doesn't take the head of a list to remain type compliant.
allGuiltyBoys :: [[Boy]]
allGuiltyBoys = filter (statementTeacher . answers) (subsequences accused)
  where
    accused = [boy | boy <- boys, not (null (accusers boy))] -- Only boys who are accused can be guilty

statementTeacher :: [Bool] -> Bool
statementTeacher bs = length (filter (==True) bs) == 3

answer :: Boy -> [Boy] -> Bool
answer Matthew g = not (elem Carl g || elem Matthew g)
answer Peter   g = elem Matthew g || elem Jack g
answer Jack    g = not (answer Matthew g) && not (answer Peter g)
answer Arnold  g = (answer Matthew g && not (answer Peter g)) || (not (answer Matthew g) && answer Peter g)
answer Carl    g = (answer Matthew g && answer Peter g) || (not (answer Matthew g) && not (answer Peter g))

answers :: [Boy] -> [Bool]
answers g = [answer boy g | boy <- boys]

exercise8 :: IO ()
exercise8 = do
  putStrLn $ exercise 8 "Determine which boy is guilty and which boys made an honest statement."

  putStrLn "\n== The guilty boy =="
  print guilty

  putStrLn "\n== All guilty boys (Should be a list that contains only 1 list) =="
  print allGuiltyBoys

  putStrLn "\n== The honest boys (should be 3 elements long) =="
  print honest


-------------------------------------------------------------

main :: IO ()
main = do
  exercise1

  exercise2

  putStrLn "\nDone :D"
