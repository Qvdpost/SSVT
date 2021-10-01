module Exercise7 where

-- import Data.Char(ord,chr,isLetter)

import Data.Char (isLetter, ord)
import Data.Map (Map, fromList, lookup)
import Helper (exercise)
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

iban :: String -> Bool
iban cs
  | validateCountry (take 2 cs) (length cs) = (read (lettersToDigits (drop 4 cs ++ take 4 cs)) :: Integer) `mod` 97 == 1
  | otherwise = False

validateCountry :: String -> Int -> Bool
validateCountry xs len = case Data.Map.lookup xs getCountryMap of
  Just n -> n == len
  nothing -> False

lettersToDigits :: String -> String
lettersToDigits (c : cs)
  | isLetter c = show (ord c - 55) ++ lettersToDigits cs
  | otherwise = c : lettersToDigits cs
lettersToDigits [] = ""

-- Source for country-code/length below. Provided in worksheet
-- https://www.iban.com/structure

-- Converting to map from here
-- https://stackoverflow.com/questions/50304525/haskell-create-key-value-map

getCountryMap :: Map String Int
getCountryMap =
  fromList
    ( zip
        (words "AL AD AT AZ BH BY BE BA BR BG CR HR CY CZ DK DO EG SV EE FO FI FR GE DE GI GR GL GT VA HU IS IQ IE IL IT JO KZ XK KW LV LB LY LI LT LU MT MR MU MD MC ME NL MK NO PK PS PL PT QA RO LC SM ST SA RS SC SK SI ES SD SE CH TL TN TR UA AE GB VG")
        ( map
            (\x -> read x :: Int)
            (words "28 24 20 28 22 28 16 20 29 22 22 21 28 24 18 28 29 28 20 18 18 27 22 22 23 27 18 28 22 28 26 23 22 23 27 30 20 20 30 21 28 25 21 20 20 31 27 30 24 27 22 18 19 15 24 29 28 25 29 24 32 27 25 24 22 31 24 19 24 18 24 21 23 24 26 29 23 22 24")
        )
    )

test_checkAllCountries :: Bool
test_checkAllCountries = all (== True) [iban i | i <- getValidIBans]

-- Source below. Provided in worksheet
-- https://www.iban.com/structure
getValidIBans :: [String]
getValidIBans = words "AL35202111090000000001234567 AD1400080001001234567890 AT483200000012345864 AZ96AZEJ00000000001234567890 BH02CITI00001077181611 BY86AKBB10100000002966000000 BE71096123456769 BA393385804800211234 BR1500000000000010932840814P2 BG18RZBB91550123456789 CR23015108410026012345 HR1723600001101234565 CY21002001950000357001234567 CZ5508000000001234567899 DK9520000123456789 DO22ACAU00000000000123456789 EG800002000156789012345180002 SV43ACAT00000000000000123123 EE471000001020145685 FO9264600123456789 FI1410093000123458 FR7630006000011234567890189 GE60NB0000000123456789 DE75512108001245126199 GI04BARC000001234567890 GR9608100010000001234567890 GL8964710123456789 GT20AGRO00000000001234567890 VA59001123000012345678 HU93116000060000000012345676 IS750001121234563108962099 IQ20CBIQ861800101010500 IE64IRCE92050112345678 IL170108000000012612345 IT60X0542811101000000123456 JO71CBJO0000000000001234567890 KZ563190000012344567 XK051212012345678906 KW81CBKU0000000000001234560101 LV97HABA0012345678910 LB92000700000000123123456123 LY38021001000000123456789 LI7408806123456789012 LT601010012345678901 LU120010001234567891 MT31MALT01100000000000000000123 MR1300020001010000123456753 MU43BOMM0101123456789101000MUR MD21EX000000000001234567 MC5810096180790123456789085 ME25505000012345678951 NL02ABNA0123456789 MK07200002785123453 NO8330001234567 PK36SCBL0000001123456702 PS92PALS000000000400123456702 PL10105000997603123456789123 PT50002700000001234567833 QA54QNBA000000000000693123456 RO09BCYP0000001234567890 LC14BOSL123456789012345678901234 SM76P0854009812123456789123 ST23000200000289355710148 SA4420000001234567891234 RS35105008123123123173 SC52BAHL01031234567890123456USD SK8975000000000012345671 SI56192001234567892 ES7921000813610123456789 SD8811123456789012 SE7280000810340009783242 CH5604835012345678009 TL380010012345678910106 TN5904018104004942712345 TR320010009999901234567890 UA903052992990004149123456789 AE460090000000123456789 GB33BUKB20201555555555 VG21PACG0000000123456789"

-- Hoare Test that performs a mutation to a valid input, that should render it invalid
preMutation :: String -> Bool
preMutation s = iban s

postMutation :: String -> Bool
postMutation s = not $ iban s

mutate :: String -> String
mutate s = s ++ ['0']

testMutations :: (String -> Bool) -> (String -> String) -> (String -> Bool) -> String -> Bool
testMutations pre f post = (\x -> pre x --> post (f x))

{-
  Test process:
  1. Start with list of know valid inputs
  2. Find mutation that should guarantee invalid input

  {\x -> y = mutate x} result = iban y { result == False }

  hoareTest precondition f postcondition

  Hypothetically: Automation by letting QuickCheck generate functions that satisfy the mutation precondition of altering valid input into invalid input.
-}


exercise7 :: IO ()
exercise7 = do
  putStrLn $ exercise 7 "IBAN Validator"

  putStrLn "\n== Test correct recognition of numbers =="
  putStrLn "Input/Output space coverage: All precomputed credit-card numbers (from a known to be correct source) should evaluate to true."
  putStrLn "Check [AL35202111090000000001234567 AD1400080001001234567890...] -> True"
  print test_checkAllCountries

  putStrLn "\n== Test mutation of valid credit-card numbers to be invalid =="
  putStrLn "Input space coverage: All precomputed credit-card numbers (from a known to be correct source)."
  putStrLn "Output: True."
  print (and (map (testMutations preMutation mutate postMutation) getValidIBans))


  putStrLn "\n == Comments =="
  putStrLn "\n This test can be seen as the following Hoare Test: (x -> y = mutate x) result = iban y (result == False) where iban x == True"
  putStrLn "\n That means that any mutation to x done by 'mutate' should render its result an invalid credit-card number."
  putStrLn "\n The current mutation implemented is a simple append, but removal of elements as well as swaps of neighbouring elements could also have been implemented."
  putStrLn "\n Ideally the mutations could be randomly generated such that a more wide input range could be tested for each mutation."
  putStrLn "\n E.g. append any character and test if the Hoare Triple holds, but also perform any swap. Theoretically Luhn is not fit to detect large displacement errors of individual numbers, so certain swaps should be able to fail the test, but that is a topic we did not explore."





_main :: IO ()
_main =
  exercise7