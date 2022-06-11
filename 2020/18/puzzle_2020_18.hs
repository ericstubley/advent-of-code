module Puzzle_2020_18 where

import Automation (submitAnswer)
import Parsing

-- data types
data Expr = Val Int | Add Expr Expr | Mul Expr Expr
    deriving (Eq, Ord, Show)

-- parsing
exprsP :: Parser [Expr]
exprsP = sepBy exprP newline

exprP :: Parser Expr
exprP = try (opP) <|> termP


termP :: Parser Expr
termP = (Val <$> integer) <|>
        ((Val . evaluate) <$> (between (char '(') (char ')') exprP))


advancedsP :: Parser [Expr]
advancedsP = sepBy advancedP newline

advancedP :: Parser Expr
advancedP = try (aOpP) <|> aTermP


aTermP :: Parser Expr
aTermP = (Val <$> integer) <|>
         ((Val . advanced) <$> (between (char '(') (char ')') advancedP))


opP :: Parser Expr
opP = do
    e1 <- termP
    space
    cons <- addP <|> mulP
    space
    e2 <- exprP
    return $ cons e1 e2


aOpP :: Parser Expr
aOpP = do
    e1 <- aTermP
    space
    cons <- addP <|> mulP
    space
    e2 <- advancedP
    return $ cons e1 e2


addP :: Parser (Expr -> Expr -> Expr)
addP = char '+' >> return Add


mulP :: Parser (Expr -> Expr -> Expr)
mulP = char '*' >> return Mul


-- functions
evaluate :: Expr -> Int
evaluate (Val n)     = n
evaluate (Add e1 e2) = 
    case e2 of 
        (Val n)     -> (evaluate e1) + n
        (Add f1 f2) -> evaluate $ Add intermediate f2
          where intermediate = Val $ (evaluate e1) + (evaluate f1)
        (Mul f1 f2) -> evaluate $ Mul intermediate f2
          where intermediate = Val $ (evaluate e1) + (evaluate f1)
evaluate (Mul e1 e2) = case e2 of
        (Val n)     -> (evaluate e1) * n
        (Add f1 f2) -> evaluate $ Add intermediate f2
          where intermediate = Val $ (evaluate e1) * (evaluate f1)
        (Mul f1 f2) -> evaluate $ Mul intermediate f2
          where intermediate = Val $ (evaluate e1) * (evaluate f1)


advanced :: Expr -> Int
advanced (Val n)     = n
advanced (Add e1 e2) = 
    case e2 of 
        (Val n)     -> (advanced e1) + n
        (Add f1 f2) -> advanced $ Add intermediate f2
          where intermediate = Val $ (advanced e1) + (advanced f1)
        (Mul f1 f2) -> advanced $ Mul intermediate f2
          where intermediate = Val $ (advanced e1) + (advanced f1)
advanced (Mul e1 e2) = (advanced e1) * (advanced e2)


-- mains

mainA :: IO ()
mainA = do
    (Just exprs) <- parseInput exprsP "18/input.txt"
    let answer = sum $ map evaluate exprs
    print answer
    -- result <- submitAnswer 2020 18 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just exprs) <- parseInput advancedsP "18/input.txt"
    let answer = sum $ map advanced exprs
    print answer
    -- result <- submitAnswer 2020 18 2 answer
    -- print result
    return ()
