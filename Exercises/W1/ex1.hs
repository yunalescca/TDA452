-- * Exercise 1 as provided by TDA452

-- SEK currency to Pounds
pounds :: Double -> Double

pounds sek = sek / 11

-----------------------------------------------------------------------------

-- * Converting Celcius to Fahrenheit
celToFah :: Double -> Double 

celToFah celcius = celcius * 1.8 + 32

-----------------------------------------------------------------------------

-- * The SEK/kg changes based on the quantity
price :: Double -> Double

price quantity 
    | quantity <= 10 = 3.5 * quantity
    | otherwise      = 3   * quantity

-----------------------------------------------------------------------------

-- * Average of two numbers
average :: Double -> Double -> Double

average x y = (x + y) / 2

-- Average of three numbers
average' :: Double -> Double -> Double -> Double

average' x y z = (x + y + z) / 3
--average' x y z = average (average x y) z

-----------------------------------------------------------------------------

-- * Calculate how many days in a year. A year is a leap year if it is 
-- divisible by 4.

isLeapYear :: Integer -> Bool
noOfDays   :: Integer -> Integer

isLeapYear year = mod year 4 == 0

noOfDays year   = if isLeapYear year then 366 else 365

-- Alternative version
noOfDays' year 
    | isLeapYear year = 366
    | otherwise       = 365

-----------------------------------------------------------------------------

-- * Number game

next :: Integer -> Integer

next n 
    | even n = n `div` 2
    | odd  n = n * 3 + 1


steps :: Integer -> Integer

steps 0 = 0
steps n 
    | n == 1    = 1 -- base case
    | otherwise = 1 + steps (next n)

numbers :: Integer -> [Integer]

numbers n 
    | n == 1    = [1]
    | otherwise = n : numbers (next n) -- concatenation, building a list

-----------------------------------------------------------------------------












