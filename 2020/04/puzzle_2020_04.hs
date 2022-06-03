module Puzzle_2020_04 where

import Automation (submitAnswer)
import Parsing
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- data types
data Field = BYR -- byr (Birth Year)
           | IYR -- iyr (Issue Year)
           | EYR -- eyr (Expiration Year)
           | HGT -- hgt (Height)
           | HCL -- hcl (Hair Color)
           | ECL -- ecl (Eye Color)
           | PID -- pid (Passport ID)
           | CID -- cid (Country ID)
           deriving (Eq, Ord, Show, Enum, Bounded)

type Passport = Map Field String

-- parsing
passportsP :: Parser [Passport]
passportsP = sepBy passportP newline


passportP :: Parser Passport
passportP = M.fromList <$> some entryP


entryP :: Parser (Field, String)
entryP = do
    f <- fieldP
    char ':'
    s <- many (alphaNumChar <|> char '#')
    optional (char ' ' <|> char '\n')
    return (f, s)


fieldP :: Parser Field
fieldP = try (string "byr" >> return BYR) <|>
         try (string "iyr" >> return IYR) <|>
         try (string "eyr" >> return EYR) <|>
         try (string "hgt" >> return HGT) <|>
         try (string "hcl" >> return HCL) <|>
         try (string "ecl" >> return ECL) <|>
         try (string "pid" >> return PID) <|>
         try (string "cid" >> return CID)


-- functions
isValid :: Passport -> Bool
isValid passport
    | numFields == 8          = True
    | numFields == 7 && noCID = True
    | otherwise               = False
      where numFields = M.size passport
            noCID = not $ M.member CID passport


strictValidator :: Passport -> Bool
strictValidator passport = all id merged
  where merged = M.mergeWithKey apply emptyField emptyFunction 
                    functionPassport passport
        apply k a b = Just $ a b
        emptyField m = M.mapWithKey (\k a -> if k == CID then True else False) m
        emptyFunction = const M.empty



functionPassport :: Map Field (String -> Bool)
functionPassport = M.fromList [ (BYR, byr)
                              , (IYR, iyr)
                              , (EYR, eyr)
                              , (HGT, hgt)
                              , (HCL, hcl)
                              , (ECL, ecl)
                              , (PID, pid)
                              , (CID, cid)]


byr :: String -> Bool
byr value = 1920 <= year && year <= 2002
  where year = (read value) :: Int


iyr :: String -> Bool
iyr value = 2010 <= year && year <= 2020
  where year = (read value) :: Int


eyr :: String -> Bool
eyr value = 2020 <= year && year <= 2030
  where year = (read value) :: Int


hgt :: String -> Bool
hgt value
    | units == "cm" && 150 <= height && height <= 193 = True
    | units == "in" && 59  <= height && height <= 76  = True
    | otherwise                                       = False
      where units = [last (init value), last value]
            height = (read $ init $ init value) :: Int


hcl :: String -> Bool
hcl value = head value == '#' 
         && length (tail value) == 6 
         && all isHex (tail value)
  where isHex c = isHexDigit c && (isLower c || isDigit c)


ecl :: String -> Bool
ecl value = value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]


pid :: String -> Bool
pid value = length value == 9 && all isDigit value


cid :: String -> Bool
cid value = True


countValid :: (Passport -> Bool) -> [Passport] -> Int
countValid validator passports = length $ filter validator passports


-- mains

mainA :: IO ()
mainA = do
    (Just passports) <- parseInput passportsP "04/input.txt"
    let answer = countValid isValid passports
    print answer
    -- result <- submitAnswer 2020 04 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just passports) <- parseInput passportsP "04/input.txt"
    let answer = countValid strictValidator passports
    print answer
    -- result <- submitAnswer 2020 04 2 answer
    -- print result
    return ()
