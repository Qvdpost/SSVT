
module Lab1 where
import Data.List
import Test.QuickCheck
import Debug.Trace

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
sumBunch = sum (take 101 primes)

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



-------------------Exercise 6---------------------------------
conjecture :: [Integer] -> Bool
conjecture ns = prime (product ns + 1)

testConjecture :: [Integer] -> Property
testConjecture ns = not (null ns) ==> conjecture ns

testConjecturePlural :: [Integer] -> Property
testConjecturePlural ns = length ns > 1 ==> conjecture ns

testConjecturePlural' :: Int -> Property
testConjecturePlural' n = n > 0 ==> conjecture (take n primes)

testConjectureLinearInf :: Int -> Int -> [[Integer]]
testConjectureLinearInf n m | n > 0 = if not (conjecture newPrimes) then [newPrimes] ++ testConjectureLinearInf (n-1) (m+1) else testConjectureLinearInf n (m+1)
                            | otherwise = []
                          where
                            newPrimes = take m primes

-- genPrimes :: Gen [Integer]
-- genPrimes = take (arbitrary :: Gen Integer) primes

exercise6 :: IO ()
exercise6 = do
  putStrLn $ exercise 6 "Disproves a conjecture by providing counterexamples that fail to hold true."

  putStrLn "\n== Test conjecture (Fail) =="
  putStrLn "Input/Output space coverage: TBD."
  -- quickCheck testConjecturePlural'
  print (testConjectureLinearInf 3 1)
  -- quickCheck $ forAll genPrimes testConjecturePlural

-------------------Exercise 7---------------------------------
intToDigitsSkip :: Integer -> [Integer]
intToDigitsSkip 0 = []
intToDigitsSkip n = intToDigitsSkip (n `div` 100) ++ [n `mod` 10]

intToDigits :: Integer -> [Integer]
intToDigits 0 = []
intToDigits n = intToDigitsSkip (n `div` 10) ++ [n `mod` 10]

luhnDoubling :: Integer -> Integer
luhnDoubling n | n > 5 = (n * 2) `mod` 10 + 1
               | otherwise = n * 2

luhn :: Integer -> Bool
luhn n = checkDigit == (10 - sum (doubles ++ constants) `mod` 10) `mod` 10
  where
    checkDigit = n `mod` 10
    luhnDigits = n `div` 10
    doubles = map luhnDoubling (intToDigitsSkip luhnDigits)
    constants = intToDigitsSkip (luhnDigits `div` 10)

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n = length (show n) == 15 && elem (take 2 (show n)) ["34", "37"] && luhn n
isMaster n = length (show n) == 16 && elem (take 2 (show n)) ["51", "55"] || (let inn = read $ take 6 (show n) :: Int in inn >= 222100 && inn <= 272099) && luhn n
isVisa n = elem (length (show n)) [13, 16] && head (show n) == '4' && luhn n

exercise7 :: IO ()
exercise7 = do
  putStrLn $ exercise 7 "Verification functions to test for typos in credit card numbers of 3 popular businesses using Luhn's algorithm."


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
guilty = head (filter (statementTeacher . answers) (subsequences boys))
honest = [boy | boy <- boys, answer boy guilty]

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
  putStrLn $ exercise 8 "."

  -- answerMatthew = not (elem Carl guilty || elem Matthew guilty)
  -- answerPeter = elem Matthew guilty || elem Jack guilty
  -- answerJack = not answerMatthew && not answer Peter
  -- answerArnold = (answerMatthew and not answerPeter) || (not answerMatthew and answerPeter)
  -- answerCarl = (answerMatthew and answerPeter) || (not answerMatthew && not answerPeter)

-------------------------------------------------------------

main :: IO ()
main = do
  exercise1

  exercise2
  putStrLn "\n== General Stuff =="
  -- putStrLn $ "Length [1,2,3]: " ++ show (length' [1,2,3])  
  -- putStrLn $ "10 to 20 with filters: " ++ show [ x*2 | x <- [10..20], x /= 13, x /=15, x /= 19]  
  -- putStrLn $ "\nFactorial 5: " ++ show (factorial 5)  
  putStrLn "\nDone :D"
