import Test.QuickCheck
import Data.List 
import Data.Char
import Data.Time.Clock(getCurrentTime, diffUTCTime)
import Network.HTTP.Base(urlEncode)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 1. 
xmas :: Int -> IO ()
xmas n = putStrLn (printStars n)

 
printStars n = unlines [conRep (n - m) " " ++ conRep m "* " | m <- [1..n]]
    where conRep n' = concat . replicate n' 



splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = [[]]
splitWhen p xs = 
    case span (not . p) xs of
        (s, []) -> [s] -- needs to return [[a]], or ["string"]
        (s, s') -> s : splitWhen p (drop 1 s')
                 


{-
	We need to drop the same amount of times as the property holds in the list.
	The amount of times the property will hold is given by taking length of 
	filtering the list. 
	And because of our base case, the length of our split will always be at least 1. 
-}
prop_splitWhen :: (a -> Bool) -> [a] -> Bool
prop_splitWhen p xs =
    length (splitWhen p xs) == length (filter p xs) + 1


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 2. 

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 3. 

data Sudoku = Sudoku [[Int]]
    deriving (Show)

ex = Sudoku [[3,6,0,0,7,1,2,0,0],[0,5,0,0,0,0,1,8,0], [0,0,9,2,0,4,7,0,0], 
             [0,0,0,0,1,3,0,2,8],[4,0,0,5,0,2,0,0,9],[2,7,0,4,6,0,0,0,0],
             [0,0,5,3,0,8,9,0,0],[0,8,3,0,0,0,0,6,0],[0,0,7,6,9,0,0,4,3]]

showSudoku :: Sudoku -> String 
showSudoku (Sudoku []) = "\n"
showSudoku (Sudoku (r:rows)) = 
    intersperse '|' (cr (map show r)) ++ 
    "\n" ++
    (replicate 17 '-') ++
    "\n" ++
    (showSudoku (Sudoku rows))

    where cr s = concat $ map (\x -> if x == "0" then " " else x) s


showSudoku' :: Sudoku -> String
showSudoku' (Sudoku sud) =
    unlines $ intersperse hr $ map showRow sud

    where hr = replicate 17 '-'
          showRow = intersperse '|' . map showNum
          showNum 0 = ' '
          showNum n = head (show n)




-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 4. 

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving Show



exTree = Node 2 (leafNode 1) (Node 1 (leafNode 1) (leafNode 0))
    where leafNode n = Node n Leaf Leaf


hBalanced :: Tree a -> (Int, Bool)
hBalanced Leaf = (0, True)
hBalanced tree = (depth tree, isBalanced tree)
    
    where depth Leaf = 0
          depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)

          isBalanced Leaf = True
          isBalanced (Node _ t1 t2) = True && isBalanced t1 && isBalanced t2
              && abs (depth t1 - depth t2) <= 1


instance Arbitrary Tree where 
    arbitrary = balTree 

balTree :: Gen (Tree Bool)
balTree = sized bTree 


bTree :: Int -> Gen (Tree Bool)
bTree n 
    | n <= 0 = return Leaf
    | otherwise =
        do b <- arbitrary 
           let m = n - 1
           (leftH, rightH) <- elements [(m, m - 1), (m, m), (m, m + 1)]
           leftT  <- bTree leftH
           rightT <- bTree rightH
           return (Node b leftT rightT)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

fac1 0 = 1
fac1 n = n * fac1 (n - 1)

fac2 0 r = r
fac2 n r = fac2 (n - 1) (n * r)  

fac1' = fac1 10000
fac2' = fac2 10000 1

speedTest e1 e2 =
    do t1 <- getCurrentTime
       print e1 -- forces the evaluation of e1 now
       t2 <- getCurrentTime
       print e2 -- if we don't print them, nothing will get computed
       t3 <- getCurrentTime
       putStrLn ("First expression: "  ++ show (diff t2 t1))
       putStrLn ("Second expression: " ++ show (diff t3 t2))
    
    where diff = diffUTCTime

 
{-
	First  expression: 0.479473s
	Second expression: 0.395138s
-}
























