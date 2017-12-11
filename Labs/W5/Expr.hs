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
    deriving (Eq, Show)


{-instance Show Expr where
    show = showExpr-}


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

number :: Parser Double
number = (readsP :: Parser Double)

expr, term, factor :: Parser Expr

expr = do t <- term
          ts <- zeroOrMore (do char '+'; term)
          return (foldl Add t ts)


term = do t <- factor
          ts <- zeroOrMore (do char '*'; factor)
          return (foldl Mul t ts)


factor = do n <- number
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



prop_ShowReadExpr:: Expr -> Bool
prop_ShowReadExpr e = let s       = showExpr e 
                          Just e' = readExpr s in 
    showExpr e' == s 



arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1, rNum), (1, rVar), (s, rOp), (s, rGeo)]
    where rNum = do n <- arbitrary
                    return $ Num n
                  
          rOp = do op <- elements [Mul, Add]
                   e1 <- arbExpr s'
                   e2 <- arbExpr s'
                   return $ op e1 e2

          rGeo = do op <- elements [Sin, Cos]
                    e  <- arbExpr s'
                    return $ op e

          rVar = elements $ map Var["x"]
          s' = (s `div` 2)


instance Arbitrary Expr where
    arbitrary = sized arbExpr



simplify :: Expr -> Expr
simplify (Num n) = (Num n)
simplify (Var "x") = (Var "x")

simplify (Add e1 e2) 
    | e1 == (Num 0) = simplify e2
    | e2 == (Num 0) = simplify e1
    | otherwise = (Add (simplify e1) (simplify e2))


simplify (Mul e1 e2) 
    | e1 == (Num 0) || e2 == (Num 0) = (Num 0)
    | e1 == (Num 1) = simplify e2
    | e2 == (Num 1) = simplify e1
    | otherwise = (Mul (simplify e1) (simplify e2))


simplify (Sin e) = Sin (simplify e)
simplify (Cos e) = Cos (simplify e)


prop_simplify :: Expr -> Double -> Bool
prop_simplify e x = (eval e x) == (eval (simplify e) x)





































