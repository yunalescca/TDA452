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

-------------------------------------

-- C * Add the operations of multiplication and integer division to the type Expr, 
-- and redefine the functions eval, showExpr, and size to include the new cases. 
-- What does your eval do when you divide by zero? Write one version of eval with 
-- the result type Maybe Int.

evalMaybe :: Expr -> Maybe Integer
evalMaybe (Lit n) = Just n
evalMaybe (Add e1 e2) = (+) <$> evalMaybe e1 <*> evalMaybe e2 -- opt 1
    {-
		evalMaybe (Add (Lit 1) (Lit 2)) == 
		(+) <$> evalMaybe (Lit 1) <*> evalMaybe (Lit 2) == 
		evalMaybe ((Lit 1)+) <*> evalMaybe (Lit 2) ==
		evalMaybe ((Lit 1) + (Lit 2)) ==
		evalMaybe (Lit 3) ==
		Just 3
    -}
evalMaybe (Sub e1 e2) = pure (-) <*> evalMaybe e1 <*> evalMaybe e2 -- opt 2
evalMaybe (Mul e1 e2) = fmap (*) (evalMaybe e1) <*> evalMaybe e2 -- opt 3
evalMaybe (Div e1 e2) = 
    do v1 <- evalMaybe e1
       v2 <- evalMaybe e2
       if v1 == 0 
           then Nothing
           else Just (v1 `div` v2)

-------------------------------------

-- D *Instead of adding extra constructors to the Expr datatype as in C it is possible 
-- to factor the definitions
{-data Expr = Lit Int | Op Ops Expr Expr
  data Ops  = Add | Sub | Mul | Div-}
-- Show how the functions eval, showExpr, and size are defined for this type. How would 
-- you add yet another extra operation Mod for remainder on integer division?


data Expr2 = Lit2 Integer | Op2 Ops Expr2 Expr2
data Ops = Add2 | Sub2 | Mul2 | Div2 | Mod2

eval2 :: Expr2  -> Integer
eval2 (Lit2 n) = n
eval2 (Op2 op e1 e2) = evalOp2 op (eval2 e1) (eval2 e2)

    where evalOp2 Add2 = (+)
          evalOp2 Sub2 = (-)
          evalOp2 Mul2 = (*)
          evalOp2 Div2 = div
          evalOp2 Mod2 = mod

showExpr2 :: Expr2 -> String
showExpr2 (Lit2 n) = show n
showExpr2 (Op2 op e1 e2) = "(" ++ showExpr2 e1 ++ op2 op ++ showExpr2 e2 ++ ")"

    where op2 Add2 = " + "
          op2 Sub2 = " - "
          op2 Mul2 = " * "
          op2 Div2 = " / "
          op2 Mod2 = " % "   


size2 :: Expr2 -> Int
size2 (Lit2 _) = 0
size2 (Op2 op e1 e2) = 1 + size2 e1 + size2 e2

-----------------------------------------------------------------------------

-- * INTEGER TREES

-- A tree is either nil or a node followed by two trees
data NTree = NilT
           | Node Int NTree NTree

-- Sum of all nodes
sumTree :: NTree -> Int
sumTree NilT           = 0
sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

-- Depth of tree
depth :: NTree -> Int
depth NilT           = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

-------------------------------------

-- A * A. Give a calculation of
    -- sumTree (Node 3 (Node 4 NilT NilT) NilT)
    -- depth (Node 3 (Node 4 NilT NilT) NilT)


{-
	sumTree (Node 3 (Node 4 NilT NilT) NilT) == 
		3 + sumTree (Node 4 NilT NilT) + sumTree NilT == 
		3 + (4 + sumTree NilT NilT) + 0 == 
		3 + (4 + 0 + 0) + 0 ==
		7

	depth (Node 3 (Node 4 NilT NilT) NilT) == 
		1 + max (depth (Node 4 NilT NilT)) (depth NilT) == 
		1 + max (1 + max (depth NiLT) (depth NilT)) 0 == 
		1 + max (1 + max 0 0) 0 == 
		1 + max (1 + 0) 0 == 
		1 + max 1 0 == 
		1 + 1 == 
		2
-}






