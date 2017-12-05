module LearnYouAHaskell where

import Data.List 
import Data.Char 
import Test.QuickCheck

-- * Examples from Learn You A Haskell

-- GETTING STARTED

head' = head [1,2,3] -- prints 1

tail' = tail [1,2,3] -- prints [2,3]

last' = last [1,2,3] -- prints 3

init' = init [1,2,3] -- prints [1,2]


take' n = take n [1,2,3] -- prints the first n element from the list

drop' n = drop n [1,2,3] -- drops  the first n element from the list

-- tail [1..20] == drop 1 [1..20] , only difference is tail fails on the empty list
-- head [1..20] == take 1 [1..20]


elem' n = n `elem` [1,2,3,4] -- checks if n is an element in the list


range1 = [1..20]
range2 = [2,4..20]
range3 = [(-1)..(-5)] -- prints empty list


cycle'  n = take n (cycle [1,2,3]) -- cycle loops a list infintely 

repeat' n = take n (repeat 5) -- repeat loops an element infintely

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- TYPES AND TYPECLASSES

-- removes non-uppercase letters from a string
removeNotUpperCase string = [ c | c <- string, c `elem` ['A'..'Z']]

onlyEvenNumbers xxs = [ [ x | x <- xs, even x ] | xs <- xxs]


zip1  = zip [1,2,3,4,5] [6,7,8,9,10]
zip2  = zip [1,2,3,4,5] [6,7,8]
zip3' = zip [1..]       ["Orange", "Apple", "Pear", "Mango"]

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- SYNTAX IN FUNCTIONS

-- when pattern matching, use the @ notation, that way you can write 
-- 'all' instead of (x:xs). 'all' is just an arbitrary word
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- to bind several variables inline with 'let'

lets = let a = 1; b = 2; c = 3 in a + b + c

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- RECURSION

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- HIGHER ORDER FUNCTIONS

-- Partially applied function
-- max 4 5 == (max 4) 5 (max :: a -> (a -> a))
max' x = max x

{-
	Technically in Haskell, one function only takes one parameter.
	So with max 4 5, this is technically (max 4) 5, and comparing the type
	signatures:
	- max :: Ord a => a -> a -> a 
	- max :: Ord a => a -> (a -> a)
	which means that max 4 runs the function on 4, returns a function, and then
	runs 5 on that function.

	All functions with several parameters are called CURRIED FUNCTIONS
	"Currying is the process of transforming a function that takes multiple 
	arguments into a function that takes just a single argument and returns 
	another function if any arguments are still needed."
-}

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])


applyTwice :: (a -> a) -> a -> a -- indicates the first parameter is a function
applyTwice f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


--       zip [1..] "hello" == [(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')]
-- flip' zip [1..] "hello" == [('h',1),('e',2),('l',3),('l',4),('o',5)]
-- flip applies a function to two arguments, but flips the arguments
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y


-- Lambdas are expressions
-- Lambdas can take several arguments    *a*         *b*
-- Its argument are all variables to the left of ->  
funs = zipWith (\a b -> (a + 1) / b) [5,4,3,2,1] [1,2,3,4,5] 



-- A fold takes a binary function, a starting value (I like to 
-- call it the accumulator) and a list to fold up. The binary function itself
-- takes two parameters

-- First let's take a look at the foldl function, also called the left fold. It 
-- folds the list up from the left side. The binary function is applied between 
-- the starting value and the head of the list. That produces a new accumulator 
-- value and the binary function is called with that value and the next element, etc.

-- Here the accumulator is at first, and x is the first element in the list
-- Also the same as 
-- 		sum' = foldl (+) 0
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs


































