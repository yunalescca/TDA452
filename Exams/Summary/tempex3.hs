
import Test.QuickCheck
import Data.List
import Data.Char

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 0. (*) Basic IO

-- | Define a function that reads an integer (n) , then reads n integers and writes
--   their sum

main0A :: IO ()
main0A = 
    do putStrLn "Gimmeh a number!"
       n <- getLine 
       let n' = (read n) :: Integer
       if n' == 0 
           then putStrLn "You're boring :("
           else do putStrLn ("Alrighty! Now input " ++ n ++ " new numbers!")
                   s <- nInput n'
                   putStr "The sum is: "
                   print s
                   return ()

nInput :: Integer -> IO Integer
nInput n = 
    do if n == 0
           then return 0
           else do i <- getLine
                   let i' = (read i) :: Integer
                   rest <- nInput (n - 1)
                   return (i' + rest)



-- | Write a program which repeatedly reads input from the user until it encounters
--   a 0, at which point the program should output a sorted list of all the values

main0B :: IO ()
main0B = 
    do putStrLn ("Input as many numbers as you like! Write 0 to quit, and I'll give" 
                ++ " back a sorted list")
       s <- getList 
       print (sort s)

getList :: IO [Integer]
getList = 
    do i <- getLine
       let i' = (read i) :: Integer
       if i' == 0
           then return []
           else do rest <- getList 
                   return (i' : rest)


-- | repeat test op has the effect of repeating op until the condition test is True.
repeats :: IO Bool -> IO () -> IO ()
repeats test op = 
    do b <- test
       if b
           then return () 
           else do op
                   repeats test op

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 1. (*) Properties of the Look Function

-- | From prelude: 
lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' x [] = Nothing
lookup' x ((x',y) : xys)
    | x == x' = Just y
    | otherwise = lookup' x xys

-- | If the look function delivers Nothing, then the thing we were looking for
--   was not in the table
prop_lookNothing :: Int -> [(Int, Int)] -> Bool
prop_lookNothing x xys =
    case lookup' x xys of
        Nothing -> not $ x `elem` map fst xys
        _       -> True

-- | Not covered in lectures: a Property instead of Bool
prop_lookNothing' :: Int -> [(Int, Int)] -> Property
prop_lookNothing' x xys = 
    case lookup' x xys of
        Nothing -> label "Nothing" $ not $ x `elem` map fst xys
        _       -> label "True"    $ True 


-- | If the look function delivers a result Just y, then the pair (x, y) should
--   have been in the table
prop_lookJust :: Int -> [(Int, Int)] -> Property
prop_lookJust x xys = 
    case lookup' x xys of
        Just y -> label "Just" $ (x, y) `elem` xys
        _      -> label "True" $ True


-- | Property that combines prop_lookNothing and prop_lookJust into one
--  .&&. not covered in lecture. Combines properties, cannot use &&
prop_lookup :: Int -> [(Int, Int)] -> Property
prop_lookup x xys = 
    (prop_lookNothing' x xys) .&&. (prop_lookJust x xys)

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 2. Monadic Helper Functions
-- | sequence takes a list of instructions resulting in a value of type a, 
--   and creates one big instruction that executes all of these, gathering 
--   all results into one result list.
-- sequence' [Just 3, Just 4, Just 5] == Just [3, 4, 5]
-- sequence' [Just 3, Just 4, Just 5, Nothing] == Nothing (because Nothing >>= \_ = Nothing)
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = 
    do i  <- m
       is <- sequence' ms
       return (i:is)


mapM' :: Monad m => (a -> m b) -> [a] -> m [b] -- [m b]
mapM' func xs = sequence (map func xs)

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 3.
-- | Implement a number guessing game
game :: IO ()
game = 
    do putStrLn ("Let's play the number guessing game! Think of a number between"
              ++ " 1 and 100 and I'll try to guess it!")
       b <- guessing 1 100
       if b 
           then putStrLn "Great, I won!"
           else putStrLn "You didn't play by the rules!"

guessing :: Integer -> Integer -> IO Bool
guessing low high =
    do let guess = (low + high) `div` 2
       if (guess < 1) || (guess > 100)
           then return False
           else do
                   putStrLn ("Is it " ++ show guess ++ "?")
                   answer <- getLine
                   case answer of
                       "higher" -> guessing (guess + 1) high
                       "lower"  -> guessing low (guess - 1)
                       "yes"    -> return True
                       _        -> guessing low high
    

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 4. A Backup Script

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 5. (*) Generating Lists
-- | Should generate a list of size n, and each element generated by g
listOf' :: Integer -> Gen a -> Gen [a]
listOf' 0 _ = return []
listOf' n g = 
    do x <- g 
       rest <- listOf' (n - 1) g
       return (x : rest)


listOf2 :: Gen a -> Gen ([a], [a])
listOf2 g =  
    do n <- arbitrary :: Gen Integer 
       a <- listOf' (abs n) g
       b <- listOf' (abs n) g
       return (a, b)

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- 6. Generating Ordered Lists

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------































