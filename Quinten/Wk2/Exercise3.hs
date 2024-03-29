module Lab2 where

import Data.Char
import Helper (exercise, tupleToList)
import System.Random
import Data.Foldable
import Data.List

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

data Prop = Prop_even | Prop_1 | Prop_2 | Prop_3
            deriving (Eq, Show)

props = [Prop_even, Prop_1, Prop_2, Prop_3]

property :: Prop -> Int -> Bool
property Prop_even x = even x
property Prop_1 x = even x && x > 3
property Prop_2 x = even x || x > 3
property Prop_3 x = (even x && x > 3) || even x

instance Ord Prop where
    compare a b   = if stronger [(-10)..10] (property a) (property b) then LT else GT


-- strength =  [((a,b), stronger [(-10)..10] (property a) (property b)) | [a, b] <- choose 2 props]




exercise3 :: IO ()
exercise3 = do
  -- putStrLn $ exercise 1 "XXX"
    print ("Hi")

_main :: IO ()
_main = do
  exercise3