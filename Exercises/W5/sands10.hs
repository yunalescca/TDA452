-- * LAZINESS: USE AND CONTROLL

-----------------------------------------------------------------------------

import Data.Char
import Data.List
import Data.Time.Clock(getCurrentTime, diffUTCTime)
import Network.HTTP.Base(urlEncode)

-----------------------------------------------------------------------------

{-Date.Time.Clock utilities contain something to get the current time,
a time data type. In order to manipulate that data time we need to subtract
one time from another (diffUTCTime)-}

-----------------------------------------------------------------------------

primes :: [Integer]
primes = sieve [2..]
 
sieve (x:xs) = 
    x : sieve [y | y <- xs, y `mod` x /= 0]




-- * In order to calculate whether 29 is a prime number, we don't need to
-- test 29 on all the prime numbers up to 23, it is enough to roughly test
-- up to the square root of 29
primes' :: [Integer]
primes' = 2 : [y | y <- [3,5..], 
    and [y `mod` x /= 0 | x <- primesToRoot y]]

    where
        primesToRoot x = let r = floor . sqrt . fromInteger $ x
                         in takeWhile (<= r) primes' 

-----------------------------------------------------------------------------

speedTest e1 e2 =
    do t1 <- getCurrentTime
       print e1 -- forces the evaluation of e1 now
       t2 <- getCurrentTime
       print e2 -- if we don't print them, nothing will get computed
       t3 <- getCurrentTime
       putStrLn ("First expression: "  ++ show (diff t2 t1))
       putStrLn ("Second expression: " ++ show (diff t3 t2))
    
    where diff = diffUTCTime

-----------------------------------------------------------------------------

-- Doesn't work?
encodeLines = interact $ unlines . map urlEncode . lines

-----------------------------------------------------------------------------

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs 


sum'' :: [Integer] -> Integer
sum'' = foldl (+) 0 


-- foldl': simplify accumulator as you go along (same as seq)
sum3 :: [Integer] -> Integer
sum3 = foldl' (+) 0 -- strict version of foldl, much faster than foldl.. (opp of lazy)
-- Make sure that the expression gets simplified as we go along by using seq



-- * We compute as we go along, so we are not left with a big expression
-- in the end. 
-- "First compute this, then the recursive call"
-- We force the accumulator to be simplified on each recursive call
sum4 :: [Integer] -> Integer
sum4 = s 0
    where s acc [] = acc
          s acc (x:xs) = acc `seq` s (acc+x) xs



million = 1000000 :: Integer

-----------------------------------------------------------------------------

-- fromIntegral converts Int to Integer
-- * The problem with average, is that we would need to sum a really
-- big list, save that, compute the length of that list and then divide
-- those. What we would want to do, with lazy evaluation, is to collect a bit,
-- sum a bit, collect a bit and sum a bit and so on. However, because of Haskell's
-- garbage collection it will think that those 20 first numbers are computed
-- and done with, and then discard them because to save memory. But if those
-- are discarded, then we can't comupute the avg anymore. SPACE LEAK
average :: [Integer] -> Integer
average xs = sum' xs `div` fromIntegral (length xs)


-- * Make average use tail recursion by computing sum and length at the same time
average' :: [Integer] -> Integer
average' xs = av 0 0 xs
    where av sm len []     = sm `div` fromIntegral len
          av sm len (x:xs) = sm  `seq`
                             len `seq`
                             av (sm + x) (len + 1) xs 



-- Actually slower than average
-- seq is still quite lazy!
-- seq forces evaluation of first argument, but only as far as the
-- outermost constructor, or, just as far to prove that it has done
-- some evaluation. Outermost bit of value.
-- If your result was a pair, it would say "I have a left bracket, and
-- a right bracket, and a comma. That's definitely a pair" 
average'' :: [Integer] -> Integer
average'' = uncurry div . foldl' f (0,0)
    where f (s, len) n = (s + n, 1 + len)


--------------------------

average3 :: [Integer] -> Integer
average3 = uncurry div . foldl' f (0,0)
    where f (s, len) n = let s' = s + n
                             l' = 1 + len 
                         in s' `seq` l' `seq` (s',l') -- seq has to compute s' and l' before (s',l')


-----------------------------------------------------------------------------

-- * LAZINESS AND IO

-- * This will not work. 
-- contents <- readFile file, doesn't actually do anything except define 
-- a variable contents
-- let n = read contents is the same, we haven' actually done anything
-- with the file yet, so it is still open for reading
-- That means when we try to writeFile, we try to write at the same time
-- as we have read it.
count :: String -> IO Int 
count file = 
    do contents <- readFile file
       let n = read contents
       writeFile file (show (n+1))
       return n

-- * This will force computation of n, and then actually read the file
count' :: String -> IO Int
count' file =
    do contents <- readFile file
       let n = read contents
       n `seq` writeFile file (show (n+1))
       return n



























