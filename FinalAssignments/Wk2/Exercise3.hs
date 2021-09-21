module Exercise3 where

import Data.Char
import Helper (exercise)
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



exercise3 :: IO ()
exercise3 = do
  putStrLn $ exercise 3 "Strength of properties from WorkSheet 2"
  putStrLn $ "Create an ordinal property to allow comparing of properties"

  print (sort props)

_main :: IO ()
_main = do
  exercise3