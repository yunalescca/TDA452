-- * RECURSIVE DATA TYPES
-----------------------------------------------------------------------------

import Test.QuickCheck
import Data.Maybe(fromJust)

-----------------------------------------------------------------------------

-- Tree shaped because more than one occurrence
-- of the recursion

data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Var String -- variables
    deriving Eq

-- instead of deriving Show, we can create a new instance
-- of the Show class, and use our own show expression
instance Show Expr where
    show = showExpr

-- (1 + 2) * 4
ex1 = Mul (Add (Num 1) (Num 2)) (Num 4)

-- 1 + 2 * 4
ex2 = Add (Num 1) (Mul (Num 2) (Num 4))

-- (y + 2) * x
ex3 = Mul (Add (Var "y") (Num 2)) (Var "x")

-- x + 2 * y
ex4 = Add (Var "x") (Mul (Num 2) (Var "y"))

-- 5 + 2 * 4
ex5 = Num (-5) `Add` (Num 2 `Mul` Num 4)

-----------------------------------------------------------------------------

type Table = [(String, Integer)]

-- Before var: eval :: Expr -> Integer
eval :: Table -> Expr -> Integer
eval t e = eval' e
    where eval' (Num n)     = n
          eval' (Mul e1 e2) = eval' e1 * eval' e2 -- DRY code
          eval' (Add e1 e2) = eval' e1 + eval' e2 
          eval' (Var x)     = fromJust $ lookup x t -- lookup returns a Maybe

look k [] = error $ "No value for " ++ k
look k ((k', v): t) 
     | k == k' = v
     | otherwise = look k t

-- show
showExpr :: Expr -> String
showExpr (Var x) = x -- x already string
showExpr (Num n) = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2

showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

-----------------------------------------------------------------------------

range :: Integer
range = 4

level :: Int -- memory note
level = fromInteger range

-- Recall: the do notation works on monads, for instance
-- IO:s and Gen:s

rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum), (s,rBin s)]
    where rNum = elements $ map Num [-range..range]
          rBin s = do op <- elements [Mul, Add]
                      e1 <- rExpr s'
                      e2 <- rExpr s'
                      return $ op e1 e2
          s' = (s `div` 2)


-- Call upon: sample (arbitrary :: Gen Expr)
instance Arbitrary Expr where
    arbitrary = sized rExpr

-----------------------------------------------------------------------------

-- we are mixing up our monads. IO is one monad, and 
-- gen is another monad
{-
main :: IO ()
main = do es <- sample' $ rExpr level
          let e = es !! level
          putStrLn $ "What is the value of " ++ show e
          ans <- getLine
          let val = show $ eval e
          if (ans == val)
              then putStrLn "Correct!"
              else putStrLn $ "Fail! The correct answer was: " ++ val
          main -}
-----------------------------------------------------------------------------

--The variables in an equation
vars :: Expr -> [String]
vars (Num n) = []
vars (Var s) = [s]
vars (Add e1 e2) = vars e1 ++ vars e2
vars (Mul e1 e2) = vars e1 ++ vars e2

-----------------------------------------------------------------------------








































