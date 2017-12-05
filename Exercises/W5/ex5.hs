module Ex5 where

import Data.List
import Data.Char
import Test.QuickCheck

-----------------------------------------------------------------------------


data Expr = 
    Lit Integer
    | Add Expr Expr
    | Sub Expr Expr 
    | Mul Expr Expr -- added in 0C
    | Div Expr Expr -- added in 0C
    deriving (Eq, Show)


-----------------------------------------------------------------------------

-- | Evaluates an expression

eval :: Expr -> Integer
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2 -- added in 0C
eval (Div e1 e2) -- added in 0C
    | eval e2 == 0 = error "Divide by 0"
    | otherwise = eval e1 `div` eval e2

-----------------------------------------------------------------------------

-- | Shows an expression

showExpr :: Expr -> String
showExpr (Lit n)     = show n
showExpr (Add e1 e2) = showOp " + " e1 e2
showExpr (Sub e1 e2) = showOp " - " e1 e2
showExpr (Mul e1 e2) = showOp " * " e1 e2 -- added in 0C
showExpr (Div e1 e2) = showOp " / " e1 e2 -- added in 0C

-- Helper function to reduce code duplication
showOp :: String -> Expr -> Expr -> String
showOp op e1 e2 = "(" ++ showExpr e1 ++ op ++ showExpr e2 ++ ")"

-----------------------------------------------------------------------------

-- 0 * EXPRESSION AND INTEGER TREES

-- B * Define a function which counts the number of operations
-- in an expression

size :: Expr -> Int
size (Lit _) = 0
size (Add e1 e2) = 1 + size e1 + size e2
size (Sub e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2
size (Div e1 e2) = 1 + size e1 + size e2


-- C * Add the operations of multiplication and integer division to the type Expr, 
-- and redefine the functions eval, showExpr, and size to include the new cases. 
-- What does your eval do when you divide by zero? Write one version of eval with 
-- the result type Maybe Int.


























