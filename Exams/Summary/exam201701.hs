import Data.Char
import Data.List
import Test.QuickCheck

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 1. 

findIndices' :: (a -> Bool) -> [a] -> [Int]
findIndices' p xs = map fst $ filter (p . snd) xs'
    where xs' = zip [0..] xs


prop_findIndices :: (a -> Bool) -> [a] -> Bool
prop_findIndices p xs = and [p (xs !! pos) | pos <- findIndices' p xs]

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 2. 
split' :: [a] -> ([a], [a])
split' []       = ([], [])
split' [x]      = ([x], [])
split' (x:y:xs) = (x : xs1, y : xs2)
    where (xs1, xs2) = split' xs



merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge allx@(x:xs) ally@(y:ys) 
    | x < y = x : merge xs ally
    | otherwise = y : merge allx ys


mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort x1) (mergeSort x2)
    where mid = length xs `div` 2
          (x1, x2) = splitAt mid xs

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 3. 

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)


instance Functor Tree where 
     fmap _ Empty = Empty
     fmap func (Branch a t1 t2) =
         Branch (func a) (fmap func t1) (fmap func t2)


doubleTree :: Num a => Tree a -> Tree a
doubleTree = fmap (*2) 

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 4. 

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 5.

--parse :: Parser a -> String -> Maybe (a, String)
{-
completeParser :: Parser a -> String -> Maybe a
completeParser parser string = 
    case parser string of
        Just (a, "") -> Just a
        _            -> Nothing
-}

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 6. 

-- Generates a list of a given length n, using g
vectorOf_i,vectorOf_ii :: Int -> Gen a -> Gen [a]

-- Only standard functions
vectorOf_i n g = sequence $ replicate n g

-- Recursion directly
vectorOf_ii 0 _ = return []
vectorOf_ii n g = 
    do x <- g
       rest <- vectorOf_ii (n - 1) g
       return (x:rest)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 6. 

validCard :: Integer -> Bool
validCard n = (sum . sub) (double n') `mod` 10 == 0
    where n' = map digitToInt $ show n

sub :: [Int] -> [Int]
sub xs = map (\x -> if x > 9 then subtract 9 x else x) xs

double :: [Int] -> [Int]
double []       = []
double (x:y:xs) = x * 2 : y : double xs


























