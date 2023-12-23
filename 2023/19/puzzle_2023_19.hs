module Puzzle_2023_19 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Control.Applicative (liftA2)

-- data types
data Workflow = Workflow { _name :: String 
                         , _rules :: [Rule]}
data Rule = Rule { _constraint :: Constraint
                 , _result :: Result}
data Constraint = Empty | Constraint { _category :: Category
                                     , _op :: Char
                                     , _value :: Int}
data Category = X | M | A | S deriving (Eq, Ord, Show)
data Result = Accept | Reject | Redirect String deriving (Eq, Ord, Show)
data Part = Part { _x :: Int
                 , _m :: Int
                 , _a :: Int
                 , _s :: Int }
                    deriving (Eq, Ord, Show)

type Env = Map String Workflow

-- parsing
inputP :: Parser ([Workflow], [Part])
inputP = do
    workflows <- workflowsP
    newline
    parts <- partsP
    return (workflows, parts)

workflowsP :: Parser [Workflow]
workflowsP = sepEndBy workflowP newline

workflowP :: Parser Workflow
workflowP = do
    name <- nameP
    string "{"
    rules <- sepBy ruleP (char ',')
    string "}" 
    return $ Workflow name rules

ruleP :: Parser Rule
ruleP = do
    c <- constraintP
    r <- resultP
    return $ Rule c r


constraintP :: Parser Constraint
constraintP = try constraintP' <|> return Empty

constraintP' :: Parser Constraint
constraintP' = do
    c <- categoryP
    o <- (char '>') <|> (char '<')
    v <- natural
    char ':'
    return $ Constraint c o v
    --f <- (char '>' >> return (>) ) <|> (char '<' >> return (<))
    --v <- natural
    --char ':'
    --return $ Constraint c (\x -> f x v)


categoryP :: Parser Category
categoryP = (char 'x' >> return X)
        <|> (char 'm' >> return M)
        <|> (char 'a' >> return A)
        <|> (char 's' >> return S)

resultP :: Parser Result
resultP = (char 'A' >> return Accept)
      <|> (char 'R' >> return Reject)
      <|> (nameP >>= (\name -> return $ Redirect name))

nameP :: Parser String
nameP = count' 2 3 letterChar

partsP :: Parser [Part]
partsP = sepBy partP newline


partP :: Parser Part
partP = do
    string "{x="
    x <- natural
    string ",m="
    m <- natural
    string ",a="
    a <- natural
    string ",s="
    s <- natural
    string "}"
    return $ Part x m a s

-- functions
runWorkflows :: Map String Workflow -> Part -> Result
runWorkflows flowMap part = runner "in" part
  where runner name p
            | res == Accept = Accept
            | res == Reject = Reject
            | otherwise     = runner name' part
              where res = runWorkflow (flowMap M.! name) part
                    (Redirect name') = res


runWorkflow :: Workflow -> Part -> Result
runWorkflow (Workflow _ rules) part = runner rules part
  where runner (r:rs) p
            | applyC (_constraint r) p = _result r
            | otherwise                = runner rs p



applyC :: Constraint -> Part -> Bool
applyC Empty _ = True
applyC (Constraint cat op val) part
    | cat == X = comp $ _x part
    | cat == M = comp $ _m part
    | cat == A = comp $ _a part
    | cat == S = comp $ _s part
      where comp = comparator op val


comparator :: Char -> Int -> (Int -> Bool)
comparator op val
    | op == '<' = \x -> x < val
    | op == '>' = \x -> x > val

partSum :: Part -> Int
partSum (Part x m a s) = x + m + a + s

solveA :: [Workflow] -> [Part] -> Int
solveA workflows parts = sum . map partSum $ goods
  where flowMap = M.fromList $ zip (map _name workflows) (workflows)
        goods = filter (\p -> runWorkflows flowMap p == Accept) parts


-- functions for part B

invalid :: Part -> Part -> Bool
invalid (Part x1 m1 a1 s1) (Part x2 m2 a2 s2) = (x1 > x2)
                                             || (m1 > m2)
                                             || (a1 > a2)
                                             || (s1 > s2)

diameter :: Part -> Part -> Int
diameter lower upper
    | invalid lower upper = 0
    | otherwise         = diam lower upper


-- lower then upper
-- be careful to only use this on non-empty parts because signs could fool you
diam :: Part -> Part -> Int
diam (Part x1 m1 a1 s1) (Part x2 m2 a2 s2) = (x2 - x1 + 1)
                                           * (m2 - m1 + 1)
                                           * (a2 - a1 + 1)
                                           * (s2 - s1 + 1)


-- narrow down the parts based on the constraint so that it always satisfies
restrict :: Constraint -> Part -> Part -> (Part, Part)
restrict Empty lower upper = (lower, upper)
restrict con lower upper
    | _op con == '<' = (lower, restrict' con upper)
    | _op con == '>' = (restrict' con lower, upper)


restrict' :: Constraint -> Part -> Part
restrict' (Constraint cat op val) (Part x m a s)
    | cat == X = Part (adjust x) m a s
    | cat == M = Part x (adjust m) a s
    | cat == A = Part x m (adjust a) s
    | cat == S = Part x m a (adjust s)
      where adjust
                | op == '<' = min (val - 1)
                | op == '>' = max (val + 1)

-- find the part which doesn't get restricted
remain :: Constraint -> Part -> Part -> (Part, Part)
remain Empty lower upper = (Part 1 1 1 1, Part 0 0 0 0)
remain con lower upper
    | _op con == '<' = (remain' con lower, upper)
    | _op con == '>' = (lower, remain' con upper)

-- apply the constraint in the other direction so that it never satisfies
remain' :: Constraint -> Part -> Part
remain' (Constraint cat op val) (Part x m a s)
    | cat == X = Part (adjust x) m a s
    | cat == M = Part x (adjust m) a s
    | cat == A = Part x m (adjust a) s
    | cat == S = Part x m a (adjust s)
      where adjust
                | op == '<' = max val
                | op == '>' = min val

solveB :: [Workflow] -> Int
solveB = solveHelperB "in"

solveHelperB :: String -> [Workflow] -> Int
solveHelperB name workflows = runReader (measureWorkflow name initLower initUpper) flowMap
  where flowMap = M.fromList $ zip (map _name workflows) (workflows)


initLower :: Part
initLower = Part 1 1 1 1

initUpper :: Part
initUpper = Part 4000 4000 4000 4000

measureWorkflow :: MonadReader Env m => String -> Part -> Part -> m Int
measureWorkflow name lower upper = do
    wf <- reader (\m -> m M.! name)
    measureRules (_rules wf) lower upper


measureRules :: MonadReader Env m => [Rule] -> Part -> Part -> m Int
measureRules [] lower upper = return 0
measureRules (r:rs) lower upper = do
    if invalid lower upper then return 0 else do
        let (l1, u1) = restrict (_constraint r) lower upper
        let (l2, u2) = remain (_constraint r) lower upper
        this <- case _result r of
                    Accept        -> return $ diameter l1 u1
                    Reject        -> return 0
                    (Redirect wf) -> measureWorkflow wf l1 u1
        next <- measureRules rs l2 u2
        return $ this + next


-- mains

mainA :: IO ()
mainA = do
    (Just (workflows, parts)) <- parseInput inputP "19/input.txt"
    let answer = solveA workflows parts
    print answer
    -- result <- submitAnswer 2023 19 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just (workflows, parts)) <- parseInput inputP "19/input.txt"
    let answer = solveB workflows
    print answer
    -- result <- submitAnswer 2023 19 2 answer
    -- print result
    return ()
