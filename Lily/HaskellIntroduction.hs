import           Data.Char
import           Data.List
import           Debug.Trace
import           Lab1
import           Test.QuickCheck

plus :: Num a => a -> a -> a
plus a b = a + b

-- Proof by Induction (Fail)
sumOdds' :: Integer -> Integer
sumOdds' n = sum [ 2*k - 1 | k <- [1..n] ]

sumOdds :: Integer -> Integer
sumOdds n = n^2

testSumOdds :: Integer -> Bool
testSumOdds n = sumOdds' n == sumOdds n

testSumOdds' :: Integer -> Bool
testSumOdds' n = let a = abs n in sumOdds' a == sumOdds a

testSumOdds2' :: Integer -> Bool
testSumOdds2' n = trace ("\n" ++ show (abs n)) (let a = abs n in sumOdds' a == sumOdds a)

-- Proof by Induction (Success :D)
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

testSumOddsAlt :: Integer -> Bool
testSumOddsAlt n = n>0 --> sumOdds' n == sumOdds n

testSumOdds2 :: [Integer] -> Bool
testSumOdds2 = all (\x -> sumOdds' x == sumOdds x)

testSumOdds4' :: [Integer] -> Bool
testSumOdds4' xs = trace ("\n" ++ show xs) (all (\x -> sumOdds' x == sumOdds x) xs)

testSumOdds3' :: Integer -> Bool
testSumOdds3' n = trace ("\n" ++ show n) (sumOdds' n == sumOdds n)

uniques :: Eq a => Gen a -> Gen [a]
uniques gen = nub <$> listOf gen

-- General Haskell Stuff
length' :: [a] -> Integer
length' xs = sum [1 | _ <- xs]

length2' :: (Num b) => [a] -> b
length2' [] = 0
length2' (_:xs) = 1 + length2' xs

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

sumListL :: (Num a) => [a] -> [a]
sumListL = scanl (+) 0

sumListR :: (Num a) => [a] -> [a]
sumListR = scanr (+) 0

-- Examples
likes :: Boy -> Boy -> Bool
likes Jack Jack = True
likes _ Jack    = False
likes _ _       = True

main :: IO ()
main = do
  putStrLn "== Proof induction (Fail) =="
  quickCheckResult testSumOdds

  putStrLn "\n== Proof induction (Success) =="
  quickCheckResult testSumOdds'
  quickCheckResult $ forAll (uniques genPositiveIntegers) testSumOdds2
  quickCheckResult $ forAll genPositiveIntegers testSumOdds
  quickCheckResult testSumOddsAlt

  putStrLn "\n== General Stuff =="
  putStrLn $ "Length [1,2,3]: " ++ show (length' [1,2,3])
  putStrLn $ "10 to 20 with filters: " ++ show [ x*2 | x <- [10..20], x /= 13, x /= 15, x /= 19]

  putStrLn $ "\nFactorial 5: " ++ show (factorial 5)

  putStrLn "\nDone :D"
