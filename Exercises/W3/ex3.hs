import Data.List
import Test.QuickCheck

-----------------------------------------------------------------------------

-- 0 * Defining Functions over Lists

-- * Implement take from prelude
take' :: Int -> [a] -> [a]
take' n _  | n <= 0 = []
take' _ []          = []
take' n (x:xs)      = x : take' (n - 1) xs

prop_take n xs = take n xs == take' n xs



-- * Implement drop from prelude
drop' :: Int -> [a] -> [a] 
drop' 0 ls = ls
drop' n (x:xs) = drop' (n - 1) xs

prop_drop n xs = drop n xs == drop' n xs



-- * Solution 1
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs 
    | n <= 0        = ([], xs)
    | n > length xs = (xs, [])

splitAt' n (x:xs)   = (first, second)
    where first  = x : (fst rest)
          second =      snd rest 
          rest   = splitAt (n - 1) xs 

prop_split1 n xs = splitAt n xs == splitAt' n xs



-- * Solution 2
splitAt'' :: Int -> [a] -> ([a], [a])
splitAt'' n xs = (take' n xs, drop' n xs)

prop_split2 n xs = splitAt n xs == splitAt' n xs



zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []

zip3' (a:as) (b:bs) (c:cs) = [(a, b, c)] ++ zip3' as bs cs
-- zip3' (a:as) (b:bs) (c:cs) = (a,b,c) : zip3' as bs cs

prop_zip as bs cs = zip3 as bs cs == zip3' as bs cs

-- where takemax = minimum [length as, length bs, length cs]

-----------------------------------------------------------------------------

-- 1 * Permutations

isPermutation :: Eq a => [a] -> [a] -> Bool

-- [1,1,2] == [1,2,1] /= [1,2,2]
isPermutation [] [] = True
isPermutation [] bs = False
isPermutation (a:as) bs
    | a `elem` bs = isPermutation as (bs \\ [a])
    | otherwise = False


prop_permutation xs = isPermutation xs (reverse xs)

-----------------------------------------------------------------------------

-- 2 * Avoiding Duplicates

duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs) = x `elem` xs || duplicates xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []  = []
removeDuplicates [x] = [x]
removeDuplicates (x:xs) 
    | x `elem` xs = removeDuplicates xs
    | otherwise   = x : removeDuplicates xs

-----------------------------------------------------------------------------

-- 3 * Pascal's Triangle

-- Should compute the n:th row of Pascal's Triangle
pascal :: Int -> [Int]

pascal 0 = [1]
pascal n = []

-----------------------------------------------------------------------------

-- 4 * Erastosthenes' sieve

-- removes all multiples of m from ns
crossOut :: Int -> [Int] -> [Int]
crossOut m ns = [n | n <- ns, n `mod` m /= 0]

-- Finds all prime numbers in a list
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve (crossOut x xs)

range = [2..100]

-----------------------------------------------------------------------------

-- 5 * Number games
-- Test wether n is a prime (in range 2..100)
-- Test wether n is the sum of two primes (in range 2..100)

isPrime :: Int -> Bool
isPrime n = n `elem` sieve range

isSumOfPrimes :: Int -> Bool
isSumOfPrimes n = or [(n - p) `elem` primes | p <- primes]
    where primes = sieve range

-----------------------------------------------------------------------------

-- 6 * Occurrences in List

-- * If an element occurs in a list
-- With list comprehension
occursIn :: Eq a => a -> [a] -> Bool
occursIn x xs = or [x == y | y <- xs]

-- With recursion
occursIn2 :: Eq a => a -> [a] -> Bool
occursIn2 _ [] = False
occursIn2 x (y:ys)
    | x == y    = True
    | otherwise = occursIn2 x ys


-- All elements in xs should be in ys
allOccurIn :: Eq a => [a] -> [a] -> Bool
allOccurIn xs ys = and [occursIn x ys| x <- xs]


-- xs and ys should contain exactly the same elements
sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = allOccurIn xs ys && allOccurIn ys xs

-- sameElements xs ys = isPermutation xs ys

-- The number of occurrences of x in xs
numOfOccurrences :: Eq a =>  a -> [a] -> Int
numOfOccurrences x xs = sum [1 | y <- xs, x == y]



-- Takes a list and returns a lit of pairs with the number of 
-- occurrences of each element
bag :: Eq a => [a] -> [(a, Int)]
bag xs = removeDuplicates [(x, numOfOccurrences x xs) | x <- xs]

-----------------------------------------------------------------------------

-- 7 * Elements and positions

-- Prints the pair of each element in the list, and on which
-- positions the element occurs on
positions :: Eq a => [a] -> [(a, Int)]
positions xs = zip xs [0..length xs - 1]

-- Checks the first position that x occurs on in xs
firstPosition :: Eq a => a -> [a] -> Int
firstPosition x xs = head [snd y | y <- positions xs, fst y == x]

-- Removes the first occurrence of x in xs
remove1st :: Eq a => a -> [a] -> [a]
remove1st x (y:ys)
    | x == y = ys
    | not (x `elem` (y:ys)) = (y:ys)
    | otherwise = y : remove1st x ys 


remove :: Eq a => Int -> a -> [a] -> [a]
remove 0 _ xs = xs
remove n x xs = remove (n - 1) x (remove1st x xs)
-----------------------------------------------------------------------------

-- 8 * More list comprehensions

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x <- xs, y <- ys]

-- Pythagora's triad: a^2 + b^2 = c^2, s.t a <= b <= c <= 100
pythagora = [(a,b,c) | c <- [1..100], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]
-----------------------------------------------------------------------------




















