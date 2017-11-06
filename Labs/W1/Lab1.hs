import Test.QuickCheck

-- * Power function from lecture

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-----------------------------------------------------------------------------

-- * PART 1

{-

n^0 : 1 step

n^1 = 1 + n^0 = 1 + 1 = 2 steps

n^2 = 1 + n^1 = 1 + 1 + n^0 = 1 + 1 + 1 = 3 steps

n^3 = 1 + n^2 = 1 + 1 + n^1 = 1 + 1 + 1 + n^0 = 1 + 1 + 1 + 1 = 4 steps

*Answer* : n^k takes (k + 1) steps to execute.

-} 

-----------------------------------------------------------------------------

-- * PART 2

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product [n | _ <- [1..k]] 

-----------------------------------------------------------------------------

-- * PART 3

power2 :: Integer -> Integer -> Integer

power2 n k | k == 0 = 1
           | k < 0 = error "power: negative argument"
           | even k = (n * n) ^ (div k 2)
           | odd  k =  n * (n ^ (k - 1))

-----------------------------------------------------------------------------

-- * PART 4 
-- A) 

{-

Tests for when k = 0 
	-> This is the base case for all power functions and should result in 1
Tests for when k is positive
	-> All power functions should result in the same value when k is positive

No tests for k < 0 will be done, since this will simply 
produce an error message
-}


-- B)

prop_powers :: Integer -> Integer -> Bool

prop_powers n k = (power  n k == power1 n k) && 
                  (power1 n k == power2 n k) && 
                  (power  n k == power2 n k)


-- C)

results :: Bool

results = and [prop_powers n k | n <- [0..10], k <- [0..10]]



-- D)

-- quickCheck prop_powers failed at n = 0, k = -1 
-- NonNegative will guarantee that k >= 0, so we will only test 
-- for relevant cases

prop_powers' :: Integer -> NonNegative Integer -> Bool

prop_powers' n (NonNegative k) = (power  n k == power1 n k) &&
                                 (power1 n k == power2 n k) && 
                                 (power  n k == power2 n k) 


prop_powers2 n k = let k' = abs k in
                    power n k' == n^k'

-- equivalent to this? 

prop_powers3  n k = power n k' == n^k'
    where
        k' = abs k

{- Instead of 

prop_power'' n k = power n (abs k) == n^(abs k)

-}











