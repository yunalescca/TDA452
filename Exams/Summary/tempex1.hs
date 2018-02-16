import Test.QuickCheck
import Data.List

-- TEMP redo exercise


-- 1. (*) The Maximum Function

maxi :: Ord a => a -> a -> a
maxi x y
    | x >= y    = x
    | otherwise = y

-- | Should return either x or y, depending on which is bigger
prop_maxi :: Int -> Int -> Bool
prop_maxi x y = maxi x y == x || maxi x y == y

-- | if maxi x y returns x, x should be >= y
prop_maxi2 :: Int -> Int -> Bool
prop_maxi2 x y = 
    maxi x y == x && x >= y ||
    maxi x y == y && y >= x

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 2. Sum of Squares

sumsq :: Int -> Int 
sumsq 0 = 0
sumsq n 
    | n < 0 = error "Only for positive numbers" -- optional 
    | otherwise = n * n + sumsq (n - 1)

-- | Should not generate negative numbers, the formula won't work
prop_sumsq :: Int -> Property
prop_sumsq n = n >= 0 ==> 
     sumsq n == n * (n + 1) * (2 * n + 1) `div` 6

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 3. (*) The Towers of Hanoi

-- | How many moves will it take to move n rings?
hanoi :: Int -> Int
hanoi 0 = 0
hanoi n = 2 * hanoi (n - 1) + 1
-- hanoi n = 2^n - 1

{-
	We have three pillars : A, B and C
	If we have n rings, it will take x steps to move these from A to C, or,
	equivalently, x steps to move them from A to B.
	If we have (n + 1) rings, we can move n rings to pillar B in x steps.
		Then we move the last ring to pillar C. This takes 1 step.
		Then we move the n rings on top of that ring, onto pillar C. This takes x steps.

		Result: It took us x + 1 + x = 2 * x + 1 steps to move (n+1) rings
-}

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 4. Fibonacci Numbers

-- | fib n computes the n:th fibonacci number
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

-- | ... 
fibAux :: Int -> Int -> Int -> Int
fibAux i a b = undefined

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 5. Factors

-- | Finds the smallest factor for a number n
smallestFactor :: Int -> Int 
smallestFactor 1 = 1
smallestFactor n = last [n `div` r | r <- [1 .. n `div` 2], n `mod` r == 0]

-- | QuickCheck property. n should be divisible by its smallest factor
prop_smallestFactor n = n > 0 ==> 
    n `mod` (smallestFactor n) == 0

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 6. (*) Multiplying List Elements

-- | Function for multiplying the elements in a list
multiply :: Num a => [a] -> a
multiply []     = 1
multiply (x:xs) = x * multiply xs 

prop_multiply xs = multiply xs == product xs 

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 7. Avoiding duplicates 

-- | Checks for duplicate elements in a list
duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs)
    | x `elem` xs = True
    | otherwise   = duplicates xs 

-- | Removes duplicates from list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates all@(x:xs)
    | duplicates all = removeDuplicates xs
    | otherwise      = x : (removeDuplicates xs)


-- | QuickCheck property
-- After removal of duplicates, list should not contain more duplicates
prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 8. Testing

data Rank = Numeric Int | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

instance Arbitrary Rank where
    arbitrary = frequency [(4, rRoyal), (9, rNum)]
        
        where rNum   = fmap Numeric $ choose (2,10)
              rRoyal = elements [Jack, Queen, King, Ace]


rankBeats :: Rank -> Rank -> Bool
rankBeats (Numeric 6) Queen = True
rankBeats r1 r2 = r1 > r2


-- | Should define properties to find the above error in rankBeats,
--   but that will also return True for correct definition

prop_rankBeats = undefined
    
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 9. Defining Types

-- | Enum: can use succ and pre
data Month = Jan | Feb | Mar | Apr | May | Jun | 
             Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Show, Eq, Ord, Enum, Bounded)

nextMonth :: Month -> Month
nextMonth Dec = Jan
nextMonth m   = succ m


-- | Define a function which returns number of days in a month.
--   Assume every forth year is a leap year
daysInMonth :: Month -> Integer -> Integer
daysInMonth Feb year 
    | year `mod` 4 == 0 = 29
    | otherwise         = 28

daysInMonth month _ 
    | month `elem` [Jan, Mar, May, Jul, Aug, Oct, Dec] = 31
    | otherwise                                        = 30


-- | Define data type Data, containing year, month, day
data Date = Date {year :: Integer, month :: Month, day :: Integer}
    deriving (Show, Eq, Ord)


-- | Define function validDate, if the date lies between 1 and the number-
--   of days in the month
validDate :: Date -> Bool
validDate (Date y m d) = (d >= 1) && (d <= daysInMonth m y)
-- either validDate date = (day date) >= 1 ...
-- or     validDate (Date y m d)


-- | Define function tomorrow, which computes the date for the following day
tomorrow :: Date -> Date
tomorrow (Date y m d) 
    | not (validDate (Date y m d)) = error "Not a valid date"

    | (d == daysInMonth m y) && (nextM == Jan) 
        = Date (y + 1) nextM 1
    
    | (d == daysInMonth m y) 
        = Date y nextM 1

    | otherwise = Date y m (d + 1)

    where nextM = nextMonth m 





























