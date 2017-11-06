import Test.QuickCheck

-- Comment
{-Comment-}

-- :t toSEK    skriver ut toSEK :: Double -> Double

exchangeRate = 9.7145 -- SEK/euro

toEUR sek = sek / exchangeRate
toSEK eur = eur * exchangeRate
-- toSEK(eur) = eur * exchangeRate

--prop_exchange x  = toEUR (toSEK x) == eur 
prop_exchange2 eur = toEUR (toSEK eur) ~== eur

x ~== y = abs(x-y) < 1e-10 -- our own operator

{-
prop_exhange x = toEur (toSek x) ~== eur
	where
		x ~== y = abs(x-y) < 1e-10 
-}

-------------------------------------------------------------------------------------------------------

-- * Definition by cases

--absolute x | x >= 0 = x
--absolute x | x < 0 = - x 

absolute x | x >= 0 = x
		   | otherwise = -x

absolute2 x = if x >= 0 then x else -x


-------------------------------------------------------------------------------------------------------

-- * Definition by recursion, the power function n^k

power :: Integer -> Integer -> Integer

power n k | k == 0 = 1
		  | k  > 0 = power n (k - 1) * n -- k > 0 is a guard
		--  | k  < 0 = 1 / power n (- k) 

-- :t power    power :: (Num t, Num p, Ord t) => p -> t -> p


prop_power n k = power n k' == n ^ k'
	where
		k' = abs k


-- intersecting lines
intersect n | n == 1 = 0
            | n  > 1 = intersect (n - 1) + (n - 1)

-------------------------------------------------------------------------------------------------------

-- * Tuples

examplePair = (4, True)

exampleTriple = (4, False, "Hello")

exampleFunction (b, n, s) = if b then s else show n -- exampleFunction (False, 5, "Hello")


-------------------------------------------------------------------------------------------------------

-- * List

snacks = "Spam"
dinner = [snacks, "Fish", "Chips", snacks, snacks, "Pudding"]

-- Pattern matchning
summary :: [String] -> String -- Takes a list of strings and returns a string
summary []     = "Nothing"
summary [x]    = "Only " ++ x
summary [x, y] = x ++ " and " ++ y
summary  (x : xs) = x ++ " followed by other things, finally " ++ last xs
-- ++ is string concatenation

-------------------------------------------------------------------------------------------------------

-- * LIIIST COMPREHENSION

ex1 = [x * x | x <- [1..10] ]

doubles xs = [2 * x | x <- xs]

ex2  = [[x, y] | x <- "ABC", y <- "1234"] -- ["A1", "A2", "A3", ... ]
ex3 = [(x, y) | x <- "ABC", y <- "1234"] -- [('A', '1'), ('A', '2'), ... ]


-- Pythagora's Theoreom
pythag = [(a, b, c) | a <- [1..100], b <- [a..100], c <- [b..100], a^2 + b^2 == c^2]

{-
Prelude> :load Intro.hs
exhangeRate
toEUR 69
toSEK 9

:reload
quickCheck prop_exchange --- enkelt test som kör funktionen många gånger med random arguments-}



