
import Test.QuickCheck

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
-- 0. (*) Expression and Integer Trees

data Expr = Num Int 
          | Add Expr Expr 
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
    deriving Eq


instance Show Expr where
    show = showExpr


eval :: Expr -> Int
eval (Num n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) 
    | e2 == (Num 0) = error "Division by 0!"
    | otherwise = (eval e1) `div` (eval e2)


showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"
showExpr (Mul e1 e2) =        showExpr e1 ++ " * " ++ showExpr e2
showExpr (Div e1 e2) =        showExpr e1 ++ " / " ++ showExpr e2


-- | A. Give calculations
--   Done by hand

-------------------------------------------

-- | B. (*) Define the function which counts number of operations
size :: Expr -> Int
size (Num _)     = 0
size (Add e1 e2) = 1 + size e1 + size e2
size (Sub e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2
size (Div e1 e2) = 1 + size e1 + size e2

-------------------------------------------

-- | C (*). Add the operations of multiplication and integer division 
--   to the type Expr, and redefine the functions eval, showExpr, and 
--   size to include the new cases. What does your eval do when you divide 
--   by zero? Write one version of eval with the result type Maybe Int.

evalMaybe :: Expr -> Maybe Int
evalMaybe (Num n)     = Just n
evalMaybe (Add e1 e2) = (+)   <$> (evalMaybe e1) <*> (evalMaybe e2)
evalMaybe (Sub e1 e2) = (-)   <$> (evalMaybe e1) <*> (evalMaybe e2)
evalMaybe (Mul e1 e2) = (*)   <$> (evalMaybe e1) <*> (evalMaybe e2)
evalMaybe (Div e1 e2)
    | e2 == (Num 0) = error "Division by 0!"
    | otherwise    = (div) <$> (evalMaybe e1) <*> (evalMaybe e2)


-------------------------------------------
-------------------------------------------

-- | Integer Trees

data NTree = NilT
           | Node Int NTree NTree
    deriving (Show, Eq)

-- | Sums the value of the nodes
sumTree :: NTree -> Int
sumTree NilT = 0 
sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2


-- | Calculate the depth of the tree
depth :: NTree -> Int
depth NilT = 0
depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)

-------------------------------------------

-- | A. Give calculations of sumTree and depth
--   Done by hand

-------------------------------------------

-- | B. (*) Define functions to return left- and right-hand side sub trees

leftSubTree :: NTree -> NTree
leftSubTree NilT = NilT
leftSubTree (Node _ left _) = left 

rightSubTree :: NTree -> NTree
rightSubTree NilT = NilT
rightSubTree (Node _ _ right) = right

-------------------------------------------

-- | C. (*) Define a function to decide whether a number is an element of an NTree.
contains :: NTree -> Int ->  Bool
contains NilT _ =  False
contains (Node n t1 t2) x
    | x == n    = True
    | otherwise = contains t1 x || contains t2 x

-------------------------------------------

-- | D. Define functions to find the maximum and minimum values held in an NTree.

maxValue :: NTree -> Int
maxValue NilT = minBound :: Int
maxValue (Node n t1 t2)
    | n < maxLeft  && maxLeft  > maxRight = maxLeft
    | n < maxRight && maxRight > maxLeft  = maxRight
    | otherwise    = n

    where maxLeft  = maxValue t1
          maxRight = maxValue t2


minValue :: NTree -> Int
minValue NilT = maxBound :: Int
minValue (Node n t1 t2)
    | n > minLeft  && minLeft  < minRight = minLeft
    | n > minRight && minRight < minLeft  = minRight
    | otherwise    = n

    where minLeft  = minValue t1
          minRight = minValue t2


-------------------------------------------

-- | E. (*) A tree is reflected by swapping left and right sub-trees, 
--   recursively. Define a function to reflect an NTree. What is the result 
--   of reflecting twice? Write a QuickCheck property for that!

reflect :: NTree -> NTree
reflect NilT = NilT
reflect (Node n left right) = (Node n (reflect right) (reflect left))

prop_reflect tree  = tree == reflect (reflect tree)

prop_reflect' tree n = contains tree n == contains (reflect tree) n


-- Call upon: sample (arbitrary :: Gen NTree)
instance Arbitrary NTree where
    arbitrary = sized rTree


rTree :: Int -> Gen NTree
rTree s = frequency [(1, (return NilT)), (s, rSub)]
    where rSub = do n <- arbitrary
                    t1 <- rTree s'
                    t2 <- rTree s'
                    return $ Node n t1 t2
          s' = (s `div` 2)
-------------------------------------------

-- | F. Define the functions collapse, sort :: NTree -> [Int]
--   collapse should just list the nodes from the left tree, then the right tree
--   sort should list all nodes in ascending ordern

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------






























