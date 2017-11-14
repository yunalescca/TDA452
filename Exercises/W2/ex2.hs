-- * Exercise week 2

import Test.QuickCheck
import Data.List(sort, nub, elemIndex)
import Data.Maybe(fromJust)

-- * 1: maxi x y returns the maximum of x and y

maxi :: Ord a => a -> a -> a
maxi x y 
    | x >= y    = x
    | otherwise =  y

prop_maxi1 x y = maxi x y == max x y
prop_maxi2 x y = maxi x y == x || maxi x y == y
prop_maxi3 x y = maxi x y >= x && maxi x y >= y

-----------------------------------------------------------------------------

-- * 2: sum of squares
-- sumsq n returns 1*1 + 2*2 + ... + n*n

sumsq :: Int -> Int
sumsq 1 = 1
sumsq n =  n * n + sumsq (n - 1)

sumsq2 n = n * (n + 1) * (2 * n + 1) `div` 6

prop_sum n = n > 0 ==> sumsq n == sumsq2 n

-----------------------------------------------------------------------------

-- * 3: Hanoi puzzle
{- 

hanoi 0 = 0
hanoi 1 = 1
hanoi 2 = 3 
hanoi 3 = 7  (2 * hanoi (2) + 1)
hanoi 4 = 15 (2 * hanoi (3) + 1)

-}

hanoi :: Integer -> Integer
hanoi 0 = 0
hanoi n = 2 * hanoi (n-1) + 1

-----------------------------------------------------------------------------

-- * 4: Fibonacci numbers. Computes the n:th number in the Fib series
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-----------------------------------------------------------------------------

-- * 5: Factors. Returns the smallest factor which is not 1, for a number n
smallestFactor :: Integer -> Integer
smallestFactor 1 = 1
smallestFactor n = allFactors n !! 1

allFactors n = [x | x <- [1..n], n `mod` x == 0]

-----------------------------------------------------------------------------

-- * 6: Multiplying list elements
multiply :: Num a => [a] -> a
multiply []     = 1
multiply (x:xs) = x * multiply xs

-----------------------------------------------------------------------------

-- * 7: Avoiding duplicates
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

-- * 8: Testing

-----------------------------------------------------------------------------

-- * 9: Defining types

data Month = Jan | Feb | Mar | Apr | May | Jun |
             Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Show, Eq, Ord, Read, Bounded, Enum)


data Date = Date {year::Integer, month::Month, day::Integer}
    deriving Show

-- * Calculates how many days are in a month
daysInMonth :: Month -> Integer -> Integer
daysInMonth Feb year = if year `mod` 4 == 0 then 29 else 28
daysInMonth month _ 
    | month `elem` [Jan, Mar, May, Jul, Aug, Oct, Dec] = 31 -- instead of writing month == Jan || month = Mar .. 
    | otherwise = 30

-- * Checks whether a giving date is valid, that is if the day lies between
-- 1 and the last day of the given month
validDate :: Date -> Bool
validDate (Date year month day) = 
    day >= 1 && day <= (daysInMonth month year)

-- * Calculates tomorrow's date
tomorrow :: Date -> Date
tomorrow (Date year month day)
    | not (validDate (Date year month day)) = error "Not a valid date"

    | day == daysInMonth month year && month == Dec 
        = (Date (year + 1) Jan 1)

    | day == daysInMonth month year
        = (Date year (nextMonth month) 1)

    | otherwise = (Date year month (day + 1))


-- * Given a month, gives the following month
nextMonth :: Month -> Month
nextMonth month
    | n < 0 || n > size - 1 = error "Index not in range"
    | n == size - 1         = ((!!) listOfMonths 0)
    | otherwise             = ((!!) listOfMonths (n + 1))
        
        where n    = fromJust $ elemIndex month listOfMonths
              size = length listOfMonths
              listOfMonths = [Jan, Feb, Mar, Apr, May, Jun, 
                              Jul, Aug, Sep, Oct, Nov, Dec]


































