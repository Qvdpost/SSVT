{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}

module Exercise7 where

import Data.List
import Helper (exercise)
import System.Random
import Test.QuickCheck
import Exercise5(Rel,trClos)
import Exercise3(symClos)

transitiveAndSymmetricCommutative :: Rel a -> Bool
transitiveAndSymmetricCommutative a = sort (trClos (symClos a)) == sort (symClos (trClos a))

exercise7 :: IO ()
exercise7 = do
  putStrLn $ exercise 7 "Is the application of Transitive and Symmetric Closures commutative?"
  putStrLn $ "Two situations can occur"
  putStrLn $ "\nIf Symmetric closure is applied first before Transitive closure ->"
  putStrLn $ "The symmetric closure further re-enforces existing relations by applying a transposition to the atoms."
  putStrLn $ "\n\tEg: (1,2) -> (2,1)."
  putStrLn $ "This transposition of the atoms retains the original relation."
  putStrLn $ "As symmetric closure did not create new relations"
  putStrLn $ "the application of a transitive closure results in an output of the existing relations, which remain unaltered by the symmetric closure,"
  putStrLn $ "as if symmetric closure was or was not applied."
  putStrLn $ "This makes the resulting relations commutative in this scenario"
  putStrLn $ "\nIf Transitive closure is applied with Symmetric occuring afterwards ->"
  putStrLn $ "A Transitive closure exposes the existing relations,"
  putStrLn $ "by generation combinations of relation-pathways"
  putStrLn $ "\n\tEg: [(1,2),(2,3)] -> [(1,2),(1,3),(2,3)]."
  putStrLn $ "\nIn the above demonstration, (1,3) was exposed through the pathway of 1->2->3 and has existed before the closure application"
  putStrLn $ "A sort must be applied as the order of relations will differ based on closure application."
  putStrLn $ "The utilization of sort will not alter the existing relations, retaining their relations, enables direct comparision"
  putStrLn $ "\n∴ This results in Transitive and Symmetric closures being commutative."



_main :: IO ()
_main = do
  exercise7