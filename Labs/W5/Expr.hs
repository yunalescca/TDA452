module Expr where

import Data.List
import Data.Char
import Test.QuickCheck 
import Parsing
import Data.Maybe(fromJust)

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
showExpr (Var _) = "x"
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showExpr (Sin e)     = "sin " ++ sincosPar e 
showExpr (Cos e)     = "cos " ++ sincosPar e


-- | Helper function to add parentheses when we have addition inside
-- of a multiplication expression
showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

-- | Helper functiion to add parentheses when we have addition or
-- multiplication inside of sin or cos
sincosPar (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
sincosPar (Mul e1 e2) = "(" ++ showExpr (Mul e1 e2) ++ ")"
sincosPar e           = showExpr e



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
readExpr s = case parse expr (filter (not.isSpace) s) of
                 Just (e, "") -> Just e
                 _            -> Nothing

doubles :: Parser Double
doubles = (readsP :: Parser Double)

expr, term, factor :: Parser Expr

expr = do t <- term
          ts <- zeroOrMore (do char '+'; term)
          return (foldl Add t ts)


term = do t <- factor
          ts <- zeroOrMore (do char '*'; factor)
          return (foldl Mul t ts)


factor = do n <- doubles
            return (Num n)
            <|>
            do char '('
               e <- expr
               char ')'
               return e
               <|>
               do char 's'
                  char 'i'
                  char 'n'
                  fmap Sin factor
                  <|>
                  do char 'c'
                     char 'o'
                     char 's'
                     fmap Cos factor
                     <|>
                     do char 'x'
                        return (Var "x")



























