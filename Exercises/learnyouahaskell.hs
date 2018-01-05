module LearnYouAHaskell where

import Data.List 
import Data.Char 
import Test.QuickCheck
import qualified Data.Map as Map
import Control.Monad 

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


{- 
	zip [1..] "hello" == [(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')]
 flip' zip [1..] "hello" == [('h',1),('e',2),('l',3),('l',4),('o',5)]
 flip applies a function to two arguments, but flips the arguments
-}
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y


{-
	Lambdas are expressions
    Lambdas can take several arguments    *a*         *b*
	Its argument are all variables to the left of ->  
-}
funs = zipWith (\a b -> (a + 1) / b) [5,4,3,2,1] [1,2,3,4,5] 


-- ==FOLDS==--
{-
	A fold takes a binary function, a starting value (I like to 
	call it the accumulator) and a list to fold up. The binary function itself
	takes two parameters : the accumulator and the first (or last) element in the list.
	It will produce a new accumulator and now new first (or last) element in list
	Once we've walked over the whole list, only the accumulator remains, which is what
	we have reduced the list to.
-}

{-
	First let's take a look at the foldl function, also called the left fold. It 
	folds the list up from the left side. The binary function is applied between 
	the starting value (accumulator) and the HEAD of the list. That produces a new accumulator 
	value and the binary function is called with that value and the next element, etc.
-}

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- foldr :: Foldable t => (a -> a -> b) -> b -> t a -> b


-- Here the accumulator is at first, and x is the first element in the list
-- Also the same as 
-- 		sum' = foldl (+) 0
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs


-- The type of the accumulator value and the end result are always the same when
-- dealing with folds
elem'' :: Eq a => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys

{-
	foldr is the same as left fold, only the accumulator eats up the elements
	from the right. Also, the parameters are switch, so instead of 
	\acc x -> we have \x acc -> (accumulator is on the right side because it 
	starts from the right) (x is still the current value)
	foldr's x is the last element in the list 
-}

-- the : operator is much cheapar than the ++ one, so we usually use right fold
-- when building up a new list
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- foldl1 and foldr1 are the same, only you don't need to give them a starting
-- value. However, if you provide them with an empty list, it will crash. 
-- More implementations with fold : 

maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc) -- 

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head'' :: [a] -> a 
head'' = foldr1 (\x _ -> x) -- it would probably be 'better' with foldl! 


-- ==SCAN==--
{-
	scanl and scanr are like foldl and foldr, only they report all the intermediate
	accumulators states. The last element in scanl is the result of foldl, and
	the first element in scanr is the result of foldr
-}
scanl'' = scanl (+) 0 [3,5,2,1] -- [0, 3, 8, 10, 11]
scanr'' = scanr (+) 0 [3,5,2,1] -- [11, 8, 3, 1, 0]


-- ==FUNCTION APPLICATION==--
{-
	($) :: (a -> b) -> a -> b
	The expression on its right is applied as the parameter to the function
	on its left
-}
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- MODULES

-- ==QUALIFIED MODULED==--
{-
	If we want to import a module which has name clashes of functions already
	defined in the prelude, we can import the module as
		import qualified Data.Map
	so when we want to use Data.Map's filter function, we have to call it by
		Data.Map.filter .... and the 'filter' call will still be from Prelude
	 We can also rename the module
		import qualified Data.Map as M
		M.filter ....
-}

{-
	***foldl' and foldr'*** are stricter versions than the lazy foldl and foldr
	In foldl/r, the accumulator doesn't actually get updated while the folding
	happens, but makes a promise that it will produce a result when asked.
	This can produce a stack overflow error because it saves everything. If that
	happens we can use foldl' and foldr' instead. They compute the value as they go along
-}

{-
	***any/all*** takes a predicate and a list (a foldable) and checks if
	any or all elements in the list satisfies the predicate.
	Usually we use these instead of mapping over a list and then using and/or
-}

-- ***span/break*** returns the tuple of (takeWhile, dropWhile) / (dropWhile, takeWhile)


-- ***isInfixOf*** searches for sublist in a list
-- ***isPrefix/SuffixOf*** checks prefix/suffix

{-
	***partition*** takes a predicate and a list, and returns a tuple. (p x, not p x)
	While span and break are done once they encounter the first element that 
	doesn't and does satisfy the predicate, partition goes through the whole 
	list and splits it up according to the predicate.
-}

{-
	***find*** takes a predicate and a list and returns the first element as a Maybe,
	which satisfies the predicate
-}

{-
	***elemIndex*** returns the index of an element in a list (as a Maybe)
	***elemIndicises*** returns a list of the indicises
-}

{-
	***findIndex*** is like find, but it maybe returns the index of the first element 
	that satisfies the predicate
	***findIndicises*** returns the indices of all elements that satisfy the predicate 
	in the form of a list.
-}

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- DEFINING TYPES AND TYPE CLASSES

--These are data types, with value constructors
data Person = Person { firstName :: String  -- define firstName as a function :: Person -> String
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)   
{-
	Person {firstName = "Josefin", lastName = "Ulfenborg", 
	age = 23, height = 170.0, phoneNumber = "071", flavor = "Vanilla"
-}



-- Instead of writing
data P2 = P2 String String Int Float String String
    deriving (Show)
{-
	P2 "Josefin" "Ulfenborg" 23 170.0 "071" "Vanilla"
-}

firstName' (P2 fName _ _ _ _ _ ) = fName


{-
	Now we want a Person to always have the above parameters, but if a Person
	would have either a name, or a phone number etc, we could have written it like 
-}

data P3 = Name String String | Phone Int | Age Int
    deriving (Show) 

{-
	Type constructors are data type but also has a type parameter involved
	like Maybe a = Nothing | Just a
-}

{-
	we derive Ord if we want our types to be bigger than the one that comes after.
	so data Bool = False | True deriving (Ord)
	means that False > True
-}

{-
	when we have a datatype with more than one type constructor, but those take
	no parameters, we can make it part of the Enum typeclass
	By making it a part of the Bounded typeclass, we say that we have 
	things that have a lowest possible value and highest possible value
-}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

{-
	minBound :: Day == Monday
	bc of Enum : succ Monday == Tuesday 
-}
-- type classes such as Eq, work like Interfaces

-- == FUNCTOR ==--
-- 

{-
	Functors are for things that can be mapped over

	class Functor f where
		fmap :: (a -> b) -> f a - > f b

	Here, f is not a concrete type as in Eq a => a -> ..., like Int or String, 
	but a type constructor that takes one type parameter. 
		Like Maybe is a type constructor which takes an a.

	Lists are an instance of the Functor class:

	instance Functor [] where
		fmap = map

	instance Functor Maybe where 
		fmap f (Just x) = Just (f x)    -- f is function (a -> b)
		fmap f Nothing  = Nothing
-}


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- INPUT AND OUTPUT

{-
	***sequence***
	do a1 <- action
	   a2 <- action
	   print [a1,a2]
	==

	do as <- sequence [action, action]
	   print as
-}

{-
	***forever***
	takes an IO action and repeats it forever
-}

main1 = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l  


-- ***forM***
main2 = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- FUNCTIONALLY SOLVING PROBLEMS

-- * Reverse Polish Calculator

solveRPN :: (Read a, Num a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (x * y) : ys
          foldingFunction (x:y:ys) "+" = (x + y) : ys
          foldingFunction (x:y:ys) "-" = (x - y) : ys
          foldingFunction xs numberString = read numberString : xs


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- FUNCTORS, APPLICATIVE FUNCTORS AND MONOIDS

{-
	FUNCTORS: Basically things that you can map over. 
		fmap :: (a -> b) -> f a -> f b

		instance Functor f where
			fmap :: (a -> b) -> f a -> f b

	We can change the f to e.g Maybe - NOT Maybe a
	Because of the reason, if we mentally switch f to Maybe, we have 
		fmap :: (a -> b) -> Maybe a -> Maybe b
	Had we written Maybe a, we would have
		fmap :: (a -> b) -> Maybe a a -> Maybe a b
	which doesn't make sense, because Maybe only takes one parameter

	I/O is an instance of Functor
		instance Functor IO where  
    		fmap f action = do result <- action  
        					   return (f result) 

	Limited to just I/O, the form of fmap will thus be
		fmap :: (a -> b) -> IO a -> IO b
-}


{-
	APPLICATIVE FUNCTORS: "beefed up functors"
	
		class (Functor f) => Applicative f where  
    		pure :: a -> f a  
   			(<*>) :: f (a -> b) -> f a -> f b  

   	So any class that wants to be Applicative has to be a Functor as well

   	***pure*** We take a value and we wrap it in an applicative functor that 
   	has that value as the result inside it.

   	instance Applicative Maybe where
   		pure = Just

	*** <*> *** looks very similar to fmap. But where fmap takes a function, 
	a functor and then applies that function to the functor,
		<*> instead takes a function wrapped inside a functor, another functor,
	"extracts" the function and then maps it over the other functor

		instance Applicative Maybe where
			Nothing <*> _ = Nothing
			(Just f) <*> something = fmap f something

	Example: (Just (+3)) <*> (Just 4) == Just 7

	Use pure if you're dealing with Maybe values in an applicative context 
	(i.e. using them with <*>), otherwise stick to Just, example (these are equivalent)
		Just (+3)) <*> (Just 4) 
		pure (+3) <*> (Just 4)  <-- use this!

	pure f <*> x equals fmap f x

	Control.Applicative exports a function called <$>, which is just fmap as an infix operator. 
	Here's how it's defined:	
		(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
		f <$> x = fmap f x 

		pure f <*> x <*> y <*> ...
		fmap f x <*> y <*> ...
		f <$> x <*> y <*> z

	If the parameters weren't applicative functors but normal values, we'd write f x y z.

		ghci> (++) <$> Just "johntra" <*> Just "volta"  
		Just "johntravolta" 
			== (++) <$> Just "jontra" <*> Just "volta"
			== Just ("jontra"++) <*> Just "volta"
			== Just "jontravolta"

		ghci> (++) "johntra" "volta"  
		"johntravolta"  

	Because Char isn't a member of the Applicative type class, we could not write
		(++) <$> "johntra" <*> "volta"  


	Lists are a member of the Applicative type class, meaning we have a lot of ways
	to now write the same thing
		
		map    (+1)   [1..10]
		fmap   (+1)   [1..10]
		[(+1)] <*>  [1..10]
		[(+)] <*> [1] <*> [1..10]
			== [(1+)] <*> [1..10]
		(+1) <$> [1..10]
-}























