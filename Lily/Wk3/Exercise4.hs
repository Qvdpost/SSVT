{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise4 where

import Data.List
import Helper (exercise)
import Lecture3 (Token (TokenImpl, TokenNeg),parse,Form (Impl, Neg,Dsj), lexer)
import System.Random
import Test.QuickCheck
import Data.Maybe (fromMaybe)

-- Modified from below link
-- https://stackoverflow.com/questions/43291442/haskell-insert-an-element-on-nth-position
insertAt :: a -> Int -> [a] -> [a]
insertAt newElement i (a:as)
  | i > 0 = a : insertAt newElement (i - 1) as
  | i == 0 = newElement:as
  | i < 0 = error "Cannot insert at a negative index"
insertAt _ _ _ = []

toCNF::String->[Token]
toCNF s
    | Impl `elem` tokens = insertAt Dsj (fromMaybe (-1) (Impl `elemIndex` tokens)) (insertAt Neg (fromMaybe (-1) (Impl `elemIndex` tokens)) tokens)
    | otherwise = tokens
    where tokens = lexer s

exercise4 :: IO ()
exercise4 = do
  putStrLn $ exercise 4 "Using Random Testing Methods test previous Exercise"
  putStrLn $ "Example output: "

_main :: IO ()
_main = do
  exercise4