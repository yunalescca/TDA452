import Data.List
import Test.QuickCheck
import Data.Char

-- 1. 

data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving (Show, Eq)

t = Branch (Leaf 5) (Branch (Leaf 1) (Leaf 10)) 

-- | Need to define recursive helper function that captures the pattern
{-
	Recursive helper function must take the function and the trees
	Function of a's, not Trees, because both + and sum are computed on the a
-}

foldTree :: (a -> a -> a) -> Tree a -> a
foldTree _ (Leaf a)          = a --  the recursion goes here v
foldTree func (Branch t1 t2) = func (foldTree func t1) (foldTree func t2)

sumTree' :: Num a => Tree a -> a
sumTree' tree = foldTree (+) tree

minTree' :: Ord a => Tree a -> a
minTree' tree = foldTree min tree


sumTree :: Num a => Tree a -> a
sumTree (Leaf a)       = a
sumTree (Branch t1 t2) = (+) (sumTree t1) (sumTree t2)
-- sumTree t1 + sumTree t2

minTree :: Ord a => Tree a -> a
minTree (Leaf a)       = a
minTree (Branch t1 t2) = min (minTree t1) (minTree t2)
-- minTree 1 `min` minTree t2


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 2. Reimplement splitAt using recursion

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs
    | n <= 0         = ([], xs)
    | n >= length xs = (xs, [])

splitAt' n (x:xs) = (first, second)
    where first  = x : fst rest
          second =     snd rest
          rest   = splitAt (n - 1) xs


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = [] 
chunksOf n xs = x : chunksOf n xs'
    where (x, xs') = splitAt' n xs



prop_chunksOf n xs = n > 0 
    ==> length (chunksOf n xs) == (length xs + (n - 1)) `div` n

prop_chunksOf' n xs = n > 0 ==> 
    length xs `mod` n == 0 && length (chunksOf n xs) == length xs `div` n
    || length (chunksOf n xs) == length xs `div` n + 1


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 3. 

fa :: a -> [a]
fa x = [x,x]

fb :: Num a => Bool -> a -> a
fb x y = if x then y else (-y)

fc :: (a -> b, c -> d) -> (a, c) -> (b, d)
fc (f, g) (x, y) = (f x, g y)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 4. 

data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Eq, Show)

data Rank = Numeric Int | Jack | Queen | King | Ace
    deriving (Eq, Show)

data Card = Card {rank :: Rank, suit :: Suit}
    deriving (Show, Eq)

instance Arbitrary Card where
    arbitrary = undefined

data Hand = Empty | Add Card Hand

instance Arbitrary Hand where
    arbitrary = 
        do size  <- choose (1,6)
           cards <- vectorOf size arbitrary -- cards :: [Card]
           return (toHand (nub cards))

        where toHand :: [Card] -> Hand
              toHand [] = Empty
              toHand (c:cs) = Add c (toHand cs)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


-- 5.
framed :: String -> String
framed s = repStar ++ "\n* " ++ s ++ " *\n" ++ repStar
    
    where repStar = concat (replicate n "*")
          n = 2 + length s


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- 6.

type TagName = String

data XML = Text String | Elem TagName [XML]
    deriving (Show, Eq)


tableToXML :: [[String]] -> XML
tableToXML rows = Elem "table" $ map rowsToXML rows

    where rowsToXML row  = Elem "tr" $ map cellToXML row
          cellToXML cell = Elem "td" [Text cell]


showXML :: XML -> String
showXML (Text s) = concatMap replace s
showXML (Elem tag xml) 
    = "<" ++ tag ++ ">" ++ (concatMap showXML xml) ++ "</" ++ tag ++ ">"

replace :: Char -> String
replace '&' = "&amp;"
replace '<' = "&lt"
replace s   = [s]


renderTable :: Show a =>[[a]] -> String
renderTable  = showXML . tableToXML . map (map show) 































