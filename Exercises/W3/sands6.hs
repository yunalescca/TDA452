-- * TEST DATA GENERATORS

import Test.QuickCheck
import Data.List
import Data.Char

-----------------------------------------------------------------------------

doTwice io = 
    do a <- io
       b <- io
       return (a,b)


-- * We are not interested in printing IO / instructions
-- IO is a first class thing just as a function is

-- sample  (arbitrary :: Gen Int)
-- sample' (arbitrary :: Gen Int)

-----------------------------------------------------------------------------

evens :: Gen Int
evens = 
    do n <- arbitrary
       return $ n * 2


nats :: Gen Int
--nats = -- abs `fmap` arbitrary
nats = 
    do n <- arbitrary
       return $ abs n


-- choose
-- oneof : chooses one random generator in a list of generators
-- elements : sample' $ [1,2,3] == [2,2,3,1,2,3,2,1,2]
-- produces a list of random generators
-- frequency :: [(Int, Gen a)] -> Gen a : Int is the frequency

-----------------------------------------------------------------------------

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq)

-- new instance of Arbitrary called Suit
-- Suit was an instance of Show and Eq
-- and is now also an instance of Arbitrary
instance Arbitrary Suit where
    arbitrary = rSuit 

nonProp_suit s = s /= Hearts

-- Generates one of the given values
rSuit :: Gen Suit
rSuit = elements [Spades, Hearts, Diamonds, Clubs] 





data Rank = Numeric Int | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

instance Arbitrary Rank where
    arbitrary = rRank

-- Since there are 9 numeric cards and 4 royal,
-- the ratio between them should be 9:4
rRank :: Gen Rank
rRank = frequency [(4, rRoyal), (9, rNumeric)]

-- Same as rSuit
rRoyal :: Gen Rank
rRoyal = elements [Jack, Queen, King, Ace]

-- Instead of elements [2..10]
-- Generates an integer between 2 and 10 (inclusive)
-- choose :: (a,a) -> Gen a
rNumeric :: Gen Rank
rNumeric  = 
    do n <- choose (2,10) -- do knows to return a Gen Rank
       return $ Numeric n


rNumeric' = fmap Numeric $ choose (2,10)



prop_Rank (Numeric n) = n > 1 && n <= 10
prop_Rank _           = True

-- Will print the percentage of numeric cards
prop_Rank' r = classify (r < Jack) "Numeric" $ prop_Rank r


data Card = Card Rank Suit
    deriving (Show, Eq)

instance Arbitrary Card where
    arbitrary =
        do s <- arbitrary -- knows to pick an arbitrary suit
           r <- arbitrary
           return $ Card s r


-- sample' (arbitrary :: Gen Card)

-----------------------------------------------------------------------------

data Hand = Empty | Add Card Hand
    deriving Show

-- * Converting hands to lists
fromHand Empty     = []
fromHand (Add c h) = c : fromHand h

-- * Converting lists to hands
toHand = foldr Add Empty -- xs


-- * We want to create an arbitrary hand from an 
-- arbitrary list

instance Arbitrary Hand where
    arbitrary = (toHand . nub) `fmap` listOf arbitrary 


-- Hands without duplicates 

-----------------------------------------------------------------------------

-- QuickCheck gives up! 
prop_insert x xs = sorted xs ==> sorted $ insert x xs
    where types = x :: Integer

sorted xs = xs == sort xs -- inefficient!

prop_insert' x xs = sorted xs ==>
    classify (length xs < 2) "Trivial"
    $ collect (length xs)
    $ sorted 
    $ insert x xs
    where types = x :: Integer


-- data ordered list
newtype OList = OList [Integer]
    deriving (Eq, Show)

instance Arbitrary OList where
    arbitrary = 
        do is <- arbitrary
           return $ OList $ sort is



prop_insert'' x (OList xs) = 
    classify (length xs < 2) "Trivial"
    $ collect (length xs)
    $ sorted 
    $ insert x xs
    where types = x :: Integer

-----------------------------------------------------------------------------
























