import Test.QuickCheck
import Data.List

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 0. (*) Defining Functions Over Lists

-- | From notes: possible definition of take is 
take' :: Int -> [a] -> [a]
take' _ []         = []
take' n _ | n <= 0 = []
take' n (x:xs)     = x : take' (n - 1) xs

-- | Use this to define prelude function drop and splitAt
drop' :: Int -> [a] -> [a]
drop' _ []          = []
drop' n xs | n <= 0 = xs
drop' n (x:xs)      = drop (n - 1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take' n xs, drop' n xs)


-- | Define zip3 from prelude. 
--   Write one recursive solution, and one solution using the zip function 
zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' _ _ [] = []
zip3' _ [] _ = []
zip3' [] _ _ = []
zip3' (a:as) (b:bs) (c:cs) = 
    (a, b, c) : zip3' as bs cs


zip3'' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3'' _ _ [] = []
zip3'' _ [] _ = []
zip3'' [] _ _ = []
zip3'' as bs cs =
    map morph $ zip (zip as bs) cs


morph :: ((a, b), c) -> (a, b, c)
morph ((x, y), z) = (x, y, z)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 1. Permutations
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = 
    xs \\ ys == [] && 
    ys \\ xs == []


prop_permutation xs ys = 
    (isPermutation xs ys) == (isPermutation (reverse xs) (reverse ys))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 2. Duplicates
-- | Repeated from last week

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 3. Pascal's Triangle

pascal :: Int -> [Int]
pascal 1 = [1]
pascal n = 1 : nextRow (pascal (n - 1))

-- | Calculates the next row in pascal's triangle 
--   except for the first 1
nextRow :: [Int] -> [Int]
nextRow [x] = [x] -- adding the final 1
nextRow (x:y:xs) = (x + y) : (nextRow (y:xs))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 4. Erastosthenes' sieve

numbers :: [Int]
numbers = [2..100]

-- | Define recursive function, sieve, which applies Eratosthenes' sieve to 
--   the list of numbers it is given, and returns a list of all the prime 
--   numbers that it found
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : (sieve (crossOut x xs))

-- | crossOut m ns should remove all multiples of m in ns.
--   Do this with a list comprehension!
crossOut :: Int -> [Int] -> [Int]
crossOut m ns = [n | n <- ns, n `mod` m /= 0 ]

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 5. Number Games

-- | Test whether n is a prime (range 2-100)
isPrime :: Int -> Bool
isPrime n = n `elem` sieve numbers

-- | Test whether n is a sum of two primes (range 2-100)
isSumOfPrimes :: Int -> Bool
isSumOfPrimes n = or [(n - p) `elem` primes | p <- primes]
    where primes = sieve numbers

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 6. (*) Occurrences in Lists

-- | Returns True if x is an element of xs
occursIn :: Eq a =>  a -> [a] -> Bool
occursIn x xs = x `elem` xs

-- With list comprehension:
occursIn' :: Eq a => a -> [a] -> Bool
occursIn' x xs = or [x == x' | x' <- xs]


-- | Returns True if all of the elements of xs are also
--   elements of ys
allOccursIn :: Eq a => [a] -> [a] -> Bool
allOccursIn xs ys = and [occursIn x ys | x <- xs]


-- | Returns True if xs and ys have exactly the same elements
sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = allOccursIn xs ys && allOccursIn ys xs

sameElements' :: Eq a => [a] -> [a] -> Bool
sameElements' xs ys = isPermutation xs ys


-- | Returns number of occurrences of x in xs
numOfOccurrences :: Eq a => a -> [a] -> Int
numOfOccurrences x xs = sum [1 | x' <- xs, x' == x]
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 7. Elements and Positions

-- | Converts a list into a list of pairs of elements and their positions.
positions :: [a] -> [(a, Int)]
positions xs = zip xs [1..]


-- | Returns the first position at which x occurs in xs
firstPosition :: Eq a => a -> [a] -> Int
firstPosition x xs = if list == [] 
                         then (-1) 
                         else head list
    where list = [i | (e, i) <- positions xs, e == x]

{-}
remove1st x xs, which removes the first occurrence of x from xs.
 For example, remove1st 'l' "hello" == "helo"-}

-- | Removes the first occurence of x from xs
remove1st :: Eq a => a -> [a] -> [a]
remove1st x all@(x':xs)
    | numOfOccurrences x all == 0 = all
    | x == x' = xs
    | otherwise = x' : (remove1st x xs)

-- | Removes the first n occurrences of x from xs
remove :: Eq a => Int -> a -> [a] -> [a]
remove 0 _ xs = xs
remove n x all@(x':xs) = remove (n - 1) x (remove1st x all)
 
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 8. (*) More List Comprehensions

-- | Given from notes: 
pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = [(x, y) | x <- xs, y <- ys]

-- | Find all Pythagorean triads with a <= b <= c <= 100

triads :: [(Int, Int, Int)]
triads = [(a, b, c) | c <- [1..100], b <- [1..c], a <- [1..b],  
                      a^2 + b^2 == c^2]
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------




























