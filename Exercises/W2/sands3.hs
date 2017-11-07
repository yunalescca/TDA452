import Test.QuickCheck
import Prelude hiding ((++), reverse, drop, take)
import qualified Prelude as P((++), reverse, drop, take)
import Data.List(sort)

-- * Lists
-- Can hide existing functions by the import hiding ^
-- Can access the original functions by the qualified import, with P.reverse[1,2] and so on

-- * append (++) (infix brackets because of brackets)
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

----------------------------------------------------------------------

-- * reverse 
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x] -- Ord(n^2)

-- It takes quadratic time because we append for each computation
-- Use a helper function

-- More efficient
rev :: [a] -> [a]
rev xs = revinto [] xs
    where 
        revinto a []     = a
        revinto a (y:ys) = revinto (y:a) ys

----------------------------------------------------------------------

-- * take, drop

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ []         = []
take n (x:xs)     = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ []          = []
drop n (x:xs)      = drop (n - 1) xs

prop_take  n xs = abs (length (take n xs)) <= abs n

-- can also filter out tests we don't want. Discards negative tests, takes longer to perform
prop_take' n xs = n >= 0 ==> collect (n > length xs) $
                             length (take n xs) <= n

prop_takedrop n xs = n >= 0 ==> classify (n <= 0 || n > length xs) "extreme" $ 
                                take n xs ++ drop n xs == xs
    where types = xs :: [Bool] -- we only try on Bools

----------------------------------------------------------------------

-- * Sorting

-- sort :: Ord a => [a] -> [a]

-- quicksort will take at most O(n^2)
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger
    where 
        smaller = [y | y <- xs, y <  x]
        bigger  = [z | z <- xs, z >= x]

prop_qsort xs = sort xs == qsort xs
    where
        types = xs :: [Bool]


-- insertion sort
isort [] = []
isort (x:xs) = insert x (isort xs)

-- assume list is already sorted
insert :: Ord a =>  a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys) 
    | x <= y    = x : y : ys
    | otherwise = y : insert x ys

-- We want to randomly generate sorted list
prop_insert x xs = insert x (sort xs) == sort (x:xs)
    -- to similar to definition (next week)

----------------------------------------------------------------------

-- * Higher order functions

mapeven = map even [1,2,3,4,5] -- [False, True, F, T, F]

filtereven = filter even [1,2,3,4,5] -- [2,4]

sum'     xs = foldr (+)  0    xs
product' xs = foldr (*)  1    xs
and'     xs = foldr (&&) True xs
--concat'  xs = foldr (++) ||   xs
--maximum' (x:xs) = foldr max  x    xs 
























