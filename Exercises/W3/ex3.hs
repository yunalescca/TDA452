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

-- * Permutations

isPermutation :: Eq a => [a] -> [a] -> Bool













































