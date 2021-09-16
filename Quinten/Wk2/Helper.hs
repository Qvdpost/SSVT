module Helper where


tupleToList :: (Integer, Integer, Integer, Integer) -> [Integer]
tupleToList (a,b,c,d) = [a,b,c,d]

exercise :: Integer -> String -> String
exercise x name
  | x == 1 = banner
  | otherwise = "\n" ++ banner
  where
    contents = " | Exercise " ++ show x ++ ": " ++ name ++ "| "
    len = length contents -4
    delimiter = "=|" ++ replicate len '-' ++ "|="
    banner = delimiter ++ "\n" ++ contents ++ "\n" ++ delimiter ++ "\n"