-- * PARSING EXPRESSIONS

-----------------------------------------------------------------------------

import Test.QuickCheck
import Data.List
import Data.Char
import Data.Maybe

-----------------------------------------------------------------------------

type Parser a = String -> Maybe (a, String)

data Expr = 
    Num Integer 
    | Add Expr Expr 
    | Mul Expr Expr 
    deriving (Eq, Show)

{-reads "123xxx" :: [(Integer, String)]
 == [(123, "xxx)]-}

-----------------------------------------------------------------------------

-- Pattern matching on expressions: case expression
-- All patterns must line up
-- This functions returns Parser a, where a == Expr,
-- so the first tuple in Maybe (a, String) will be an Expr
num :: Parser Expr
num s = case reads s of
            (i,s'):_ -> Just (Num i, s')
            _        -> Nothing



-- warm up: parse n1 + n2 + ... + nk for k >= 1
-- Pattern matching on '+' for one or more numbers
expr1 s = chain num '+' Add s

-- GRAMMAR:
-- <expression> :: = <term> | <term> "+" <expression>
-- The full expression
-- could remove the s from both sides
expr s = chain term '+' Add s


-- GRAMMAR:
-- <term> ::= <factor> | <factor> "*" <term>
-- Pattern matching on '*'
term s = chain factor '*' Mul s

-- GRAMMAR
-- <factor> :: = "(" <expression> ")" | <number
factor ('(': s) = case expr s of
                        Just (e, ')':s') -> Just (e, s')
                        _                -> Nothing

factor s        = num s -- if it doesn't start with left bracket

-- parser char function
-- Haskell doesn't allow chain p c f s, and then Just (n, c:s)
-- because it overshadows the old value. So we need to name it
-- c' and then use guards
chain p c f s = case p s of
        Just (n, c':s') | c == c' -> case chain p c f s' of
                                   Just(e, s'') -> Just (f n e, s'')
                                   Nothing      -> Just (n, c:s')
        result                    -> result



-- top level parser
-- remove spaces and expect no junk!
-- Takes a string and maybe gives you an expression
-- If the returned expression would be "Add (Num 1) (Num2) "xxx",
-- then this function will remove the junk "xxx"
readExpr :: String -> Maybe Expr
readExpr s = case expr (filter (not.isSpace) s) of
                 Just (e, "") -> Just e
                 _            -> Nothing


-----------------------------------------------------------------------------

showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2

showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

-----------------------------------------------------------------------------

rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum), (s,rOp)]
    where rNum = do n <- arbitrary
                    return $ Num n
                  
          rOp = do op <- elements [Mul, Add]
                   e1 <- rExpr s'
                   e2 <- rExpr s'
                   return $ op e1 e2
          s' = (s `div` 2)


-- Call upon: sample (arbitrary :: Gen Expr)
instance Arbitrary Expr where
    arbitrary = sized rExpr


-- Fails because of association
-- If we don't care... 
prop_readExpr e  = let s = showExpr e in
    readExpr s == Just e

prop_readExpr2 e = let s       = showExpr e 
                       Just e' = readExpr s in 
    showExpr e' == s -- e' doesn't include the Just from above!

-----------------------------------------------------------------------------



































