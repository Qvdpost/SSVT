{--
Group Members
  Quinten Van Der Post
  Lily O'Sullivan
  Sun Li
  Arjen Swartsenburg
--}
module Helper where

exercise :: Integer -> String -> String
exercise x name
  | x == 1 = banner
  | otherwise = "\n" ++ banner
  where
    contents = " | Exercise " ++ show x ++ ": " ++ name ++ "| "
    len = length contents -4
    delimiter = "=|" ++ replicate len '-' ++ "|="
    banner = delimiter ++ "\n" ++ contents ++ "\n" ++ delimiter ++ "\n"