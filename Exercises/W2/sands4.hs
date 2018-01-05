-- * Sands Lecture 4
-- Higher-Order Functions

import Test.QuickCheck
import Data.Char(isSpace)
import Data.List(sort, group, groupBy)

-----------------------------------------------------------------------------

-- * map and filter are higher-order functions
mapeven = map even
map' f xs = [f x | x <- xs]

filtereven = filter even
filter' p xs = [x | x <- xs, p x]

-----------------------------------------------------------------------------

-- * Replace the end of the list with a base case
-- Combining operator and a base case for HOF
-- Takes the cons and applies an operator on it

sum' []    = 0
sum' (n:ns) = n + sum' ns

and' []     = True
and' (b:bs) = b && and' bs

-----------------------------------------------------------------------------

-- * Takes an operator, a basecase and a list
-- and applies the operator to the list
-- And when we get to the end of the list we return 
-- the base case
-- Folds the elements in a left associative way
foldr' op b []     = b
foldr' op b (x:xs) = x `op` foldr' op b xs

-----------------------------------------------------------------------------

-- * There are many ways to define functions.
-- One is simply the way we have done above, another
-- is to create ***local*** functions

verse = "A dying mosquito exclaimed,\nA chemist has poisoned my brain!\n"

-- * lines breaks the string at its newline character
-- * unlines which takes a list of lines and joins them back together with
-- a newline character in between

-- * local functione example
unlines' ss = foldr' joinNL "" ss
    where joinNL s1 s2 = s1 ++ "\n" ++ s2

-- * When you start doing copy-paste, it's a good
-- indication that a HOF is needed

takeLine "" = "" 
takeLine (c:cs) 
    | c /= '\n' = c : takeLine cs
    | otherwise = ""


takeWord (c:cs) 
    | not (isSpace c) = c : takeWord cs
    | otherwise       = "" 


-- * both of these functions work on strings, but is 
-- there something string-like about this definition?
-- Computation pattern that should work on lists of any type, 
-- all we need is a predicate to tell us which chars we should keep

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) 
    | p x       = x : takeWhile' p xs
    | otherwise = []


-- * How define takeLine using takeWhile?
-- Building a function that we need from existing function
-- One way is to create a ***lambda expression***, or an 
-- anonymous function / nameless function. Defines a function
-- without giving it a name. It's a function, described by
-- what the argument is. 

-- * Things that you take should not be the newline char
takeLine' cs = takeWhile' (\x -> x  /= '\n') cs 


-- * Other ways to build functions, Haskell has another useful
-- way to define functions, called ***sections***
-- Building functions from operators. Usually an operator has 
-- arguments on either side. 
-- Haskell defines functions which is an operator, which is given
-- one but not both of its arguments. (a ¢) b = a ¢ b

takeLine'' cs = takeWhile' (/= '\n') cs 

-----------------------------------------------------------------------------

-- * takeWhile has a partner function: dropWhile

{-

takeWhile (<10) [1..20]
== [1,2,3,4,5,6,7,8,9]

dropWhile (<10) [1..20]
== [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20] 

Functions which group these things together

span (<10) [1..20]
([1,2,3,4,5,6,7,8,9], [10,11,12,13,14,15,16,17,18,19,20])

break (<10) [1..20]
([],[1..20])

span (>10) [1..20]
([],[1..20])

break (>10) [1..20]
([1,2,3,4,5,6,7,8,9], [10,11,12,13,14,15,16,17,18,19,20])

break of p == span of not p

If the predicate is immediately false, it will return the empty list
span (>10), no elements are greater than 10 at first, so 
that will return the empty list. The rest is the whole thing
-}

-----------------------------------------------------------------------------

-- * There is a function tail, which can fail if
-- we give it the empty list. Can use ***drop 1***

lines' []  = []
lines' css = takeWhile (/= '\n') css
             : lines' (drop 1 (dropWhile (/= '\n') css))


nosh = "spam,eggs,chips,spam"

commSep []  = []
commSep css = takeWhile (/= ',') css
             : commSep (drop 1 (dropWhile (/= ',') css))


-- * HOF are needed when we have repeted patterns of computation

segments p [] = []
segments p xs = takeWhile p xs
                : segments p (drop 1 (dropWhile p xs))


lines'' []  = []
lines'' css = segments (/= '\n') css

commSep' []  = []
commSep' css = segments (/= ',')  css


-----------------------------------------------------------------------------

-- * Another way to build functions: *** Partial Application***

-- * What is its type?
f :: Char -> Bool -> String -> String
f c b s = c : s ++ show (not b) ++ s


-- * In other Languages:
-- String f (Char, Bool, String)

-- * The space character that we put between f and its argument
-- is an invisible operator. It's the function applicational operator

-- * When we apply f to an argument ('a'), the result is itself a function
-- of type of the rest of the original function
-- :t (f 'a') :: Bool -> [Char] -> [Char]
-- :t ((f 'a') True) :: [Char] -> [Char]

-- * Could have written the type like this
-- Char -> (Bool -> (String -> String))

-- The point of this is that we can provide some but not all
-- of the function's argument, and in turn we'll have another function
-- which will be the result.

-- *** Partial Application *** 
-- function x = expression e, can be rewritten as
-- function = expression

add x y = x + y
addOne = add 1 -- addOne x = add 1 x 

-----------------------------------------------------------------------------

-- * Every operator has a presedence between 0 and 9 
--, 9 being the highest

-----------------------------------------------------------------------------

-- * Example: Counting Words
-- Words is a function that works in a similary way to lines, 
-- except words split a string at space characters, whereas
-- lines split the string at newline characters

string = "hello clouds hello sky"

-- * Dot in Haskell is function composition! 

-- * group : takes a list and returns a list of list such that
-- the concatenation of the result is equal to the original
-- list. Each sublist contains equal elements. 
wordCount = putStr
          . unlines
          . map (\(w, n) -> w ++ ": " ++ show n)
          . map (\ws -> (head ws, length ws)) 
          . group
          . sort 
          . words


-- * putStr returns a string, breaks at newline character
-- * unlines takes a list of string, returns a string with newline chars in between


{-

map can be simplified as : 

map
    ((\(w, n) -> w ++ ": " ++ show n) .
     (\ws     -> (head ws, length ws)))

-}






















