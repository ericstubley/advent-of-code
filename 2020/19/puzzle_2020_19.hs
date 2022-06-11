module Puzzle_2020_19 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- data types
data Rule = Val Char 
          | Ref Int 
          | Seq Rule Rule 
          | Or Rule Rule 
          | Repeat Rule Rule Int -- count number of matches with r1, then do r2
          | Bounded Rule Int -- do at most int many matches with r
    deriving (Eq, Ord, Show)

type Rules = Map Int Rule

-- parsing
satelliteP :: Parser (Rules, [String])
satelliteP = do
    rules <- rulesP
    newline
    messages <- messagesP
    return (rules, messages) 


rulesP :: Parser Rules
rulesP = M.fromList <$> some (lineP <* optional newline)


lineP :: Parser (Int, Rule)
lineP = (,) <$> (integer <* string ": ") <*> ruleP

ruleP :: Parser Rule
ruleP = valP <|> (try orP) <|> (try refsP)

valP :: Parser Rule
valP = Val <$> between (char '"') (char '"') (char 'a' <|> char 'b')


orP :: Parser Rule
orP = do
    r1 <- refsP
    optional (char ' ')
    char '|'
    char ' '
    r2 <- refsP
    return $ Or r1 r2

refsP :: Parser Rule
refsP = foldl1 Seq <$> some (refP <* optional (char ' '))


refP :: Parser Rule
refP = Ref <$> integer


messagesP :: Parser [String]
messagesP = some (messageP <* optional newline)

messageP :: Parser String
messageP = some (char 'a' <|> char 'b')

-- functions
collapse :: Rules -> Rule
collapse rules = go (rules M.! 0)
  where go :: Rule -> Rule
        go (Val c) = Val c
        go (Ref n) = go (rules M.! n)
        go (Seq r1 r2) = Seq (go r1) (go r2)
        go (Or r1 r2) = Or (go r1) (go r2)
        go (Repeat r1 r2 n) = Repeat (go r1) (go r2) n


validate :: Rules -> String -> Bool
validate rules message = result && null end
  where (result, end) = go (collapse rules) message
        go :: Rule -> String -> (Bool, String)
        go (Val c) (x:xs) = (c==x, xs)
        go (Seq r1 r2) xs = let (res, xs') = go r1 xs in
            case res of
                True  -> go r2 xs'
                False -> (False, xs')
        go (Or r1 r2) xs
            | res1 && res2 && (xs1 == xs2) = (True, xs1)
            | res1 && res2                 = error "Bad Case :("
            | res1                         = (True, xs1)
            | res2                         = (True, xs2)
            | otherwise                    = (False, xs)
              where (res1, xs1) = go r1 xs
                    (res2, xs2) = go r2 xs
        go (Repeat r1 r2 n) xs
            | n == 0 && res1 = go (Repeat r1 r2 1) xs1
            | n == 1 && res1 = go (Repeat r1 r2 2) xs1
            | n <= 1         = (False, xs)
            | res1 && res2   = error "Panic!"
            | res1           = go (Repeat r1 r2 (n+1)) xs1
            | res2           = go (Bounded r2 (n-2)) xs2
            | otherwise      = (False, xs)
              where (res1, xs1) = go r1 xs
                    (res2, xs2) = go r2 xs
        go (Bounded r n) xs
            | null xs && n >= 0 = (True, [])
            | n <= 0            = (False, xs)
            | res               = go (Bounded r (n-1)) xs'
            | otherwise         = (False, xs)
              where (res, xs') = go r xs
        go _ xs = (False, xs)



fancify :: Rules -> Rules
fancify rules = M.insert 0 newRule rules
  where rule42 = rules M.! 42
        rule31 = rules M.! 31
        newRule = Repeat rule42 rule31 0




-- mains

mainA :: IO ()
mainA = do
    (Just (rules, messages)) <- parseInput satelliteP "19/input.txt"
    let answer = length $ filter (validate rules) messages
    print answer
    -- result <- submitAnswer 2020 19 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just (rules, messages)) <- parseInput satelliteP "19/input.txt"
    let rules' = fancify rules
    let answer = length $ filter (validate rules') messages
    print answer
    -- result <- submitAnswer 2020 19 2 answer
    -- print result
    return ()
