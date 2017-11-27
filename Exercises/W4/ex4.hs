-- * IO, TEST DATA AND PROPERTIES

module Ex4 where 

import Test.QuickCheck
import Data.List
import Data.Char

-----------------------------------------------------------------------------

-- 0 * Basic IO

-- * A) Write an IO program which will first read a positive integer, say n, 
--      and then reads n integers and writes their sum.


main0A :: IO ()
main0A = 
    do putStr "Enter an integer: "
       n <- getLine 
       let n2 = (read n) :: Int
       putStrLn ("Now enter " ++ n ++ " integer, and we'll compute the sum!")
       s <- (sumIntegers n2)
       putStr ("The sum is ")
       print s
       return ()


sumIntegers :: Int -> IO Int
sumIntegers n = 
    do i <- getLine
       let i2 = (read i) :: Int
       if n == 1
           then return i2
           else do rest <- (sumIntegers (n - 1))
                   return (i2 + rest)



-- * B) Write a program which repeatedly reads integers (one per line) until 
--      finding a zero value and outputs a sorted version of the inputs read.


main01B :: IO ()
main01B = 
    do putStrLn "Enter any amount of integers. To quit, enter 0:"
       ns <- collectInts
       putStrLn ("The ordered list is: ")
       print (sort ns)


collectInts :: IO [Int]
collectInts =
    do n <- getLine
       let n2 = read n :: Int
       if n2 == 0
           then return []
           else do ns <- collectInts
                   return (n2 : ns)


-- * C) such that repeat test op has the effect of repeating op until the 
--      condition test is True.
repeats :: IO Bool -> IO() -> IO ()
repeats test op = 
    do op
       b <- test
       if b then return () else repeats test op


-----------------------------------------------------------------------------

-- 1 * Properties of the Look Function

lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' x []           = Nothing
lookup' x ((x', y) : xys)
    | x == x'         = Just y
    | otherwise       = lookup' x xys


-- A) 
-- This one introduces property (not covered in lectures, though)
-- With a Property we can label the tests 
-- If lookup returns Nothing, then x should not be in the list
prop_lookNothing  :: Int -> [(Int, Int)] -> Property
prop_lookNothing x xys = 
    case lookup' x xys of
         Nothing -> label "Nothing" $ not result
         _       -> label "Just"    $ True
    where result = x `elem` map fst xys


-- Standard property which returns a Bool 
prop_lookNothing' :: Int -> [(Int, Int)] -> Bool
prop_lookNothing' _ [] = True
prop_lookNothing' x xys = 
    case lookup' x xys of 
         Nothing -> not result
         _       -> True
    where result = x `elem` map fst xys



-- B) 
-- if lookup returns Just y, then x should be in the list
prop_lookJust :: Int -> [(Int,Int)] -> Property
prop_lookJust x xys = 
    case lookup' x xys of
         Just y  -> label "Just" $ (x,y) `elem` xys
         _ -> label "Nothing" $ True


prop_lookJust' :: Int -> [(Int, Int)] -> Bool
prop_lookJust' _ [] = True
prop_lookJust' x xys = 
    case lookup' x xys of
         Just y  -> (x,y) `elem` xys
         Nothing -> True



-- C) 
-- Joins the two properties into one
-- To join to Properties : .&&.
prop_lookup x xys =
    prop_lookJust x xys .&&. prop_lookNothing x xys

-- To join to Bools : &&
prop_lookup' x xys = 
    prop_lookJust' x xys && prop_lookNothing' x xys

-----------------------------------------------------------------------------

-- 2 * 

-----------------------------------------------------------------------------

-- 3 * 

-----------------------------------------------------------------------------

-- 4 * 

-----------------------------------------------------------------------------

-- 5 * 

-----------------------------------------------------------------------------

-- 6 * 


-----------------------------------------------------------------------------



































