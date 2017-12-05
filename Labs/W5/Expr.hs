module Expr where

import Data.List
import Data.Char
import Test.QuickCheck 

-----------------------------------------------------------------------------

-- **A**
data Expr = Num Double 
          | Var String
          | Add Expr Expr
          | Mul Expr Expr 
          | Sin Expr
          | Cos Expr 
    deriving Eq


instance Show Expr where
    show = showExpr


-- **B**
-- | Prints the expressions as a string
-- The sin and cos expressions will always be printed with parentheses, because
-- of consistency and personal taste
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Var x) = x
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showExpr (Sin e)     = "sin(" ++ showExpr e ++ ")"
showExpr (Cos e)     = "cos(" ++ showExpr e ++ ")"


-- | Helper function to add parentheses when we have addition inside
-- of a multiplication expression
showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e



-- **C**
-- | Evaluates an expression. If this expression contains the variable x,
-- then we calculate the expression with the value for x
eval :: Expr -> Double -> Double 
eval (Num n)     _ = n
eval (Var _)     x = x
eval (Add e1 e2) x = eval e1 x + eval e2 x
eval (Mul e1 e2) x = eval e1 x * eval e2 x
eval (Sin e)     x = sin (eval e x)
eval (Cos e)     x = cos (eval e x)


-- **D**
-- | ...
readExpr :: String -> Maybe Expr
readExpr = undefined

































