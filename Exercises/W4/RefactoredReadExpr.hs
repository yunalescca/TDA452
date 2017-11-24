module RefactoredReadExpr where

import RefactoredParser
import Data.Char(isSpace)
import Data.Maybe
import Test.QuickCheck

data Expr = 
    Num Integer 
    | Add Expr Expr 
    | Mul Expr Expr 
    deriving (Eq, Show)

u = undefined


integer :: Parser Integer -- parse a natural number
integer = oneOrMore digit >*> success . read


num :: Parser Expr
num = pmap Num integer


expr = foldr1 Add `pmap` chain term (char '+')

term = foldr1 Mul `pmap` chain factor (char '*')

factor = char '(' >-> expr <-< char ')'
           +++ num


readExpr :: String -> Maybe Expr
readExpr s = case parse expr (filter (not.isSpace) s) of
                 Just (e, "") -> Just e
                 _            -> Nothing