import Test.QuickCheck


data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq)

-- Gives color of a Suit

data Colour = Red | Black
    deriving Show

-- Pattern matching is our primary tool
colour :: Suit -> Colour
colour Spades = Black
colour Clubs  = Black
colour _      = Red -- You don't care about value, and you're not going to use it

{- -- Also works: 
colour2 suit | suit == Spades || suit == Clubs = Black
             | otherwise = Red
-}

-- Numeric is a constructor, and a constructor can have an argument (Int)
data Rank = Numeric Int | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord) -- Ord is Comparable in Java. When it derives ord, the order we define the constructors is relevant

-- Jack :: Rank
-- Numeric :: Int -> Rank (it's a function)
-- Numeric 5 :: Rank


---------------------------------------------------------------------------

rankBeats :: Rank -> Rank -> Bool
-- rankBeats _ Ace = False -- Nothing beats an Ace
-- rankBeats Ace _ = True  -- Ace beats everything
rankBeats r1 r2 = r1 > r2

-- rankBeats r1 r2 OR
-- r1 `rankBeats` r2

---------------------------------------------------------------------------
-- QuickCheck does not know how to generate arbitrary Ranks, so we have to specify
-- We have defined the XOR operation

prop_rankBeats r1 r2  = r1 == r2 |+| r1 `rankBeats` r2 
                                 |+| r2 `rankBeats` r1

infixr 2 |+| -- infix right 
True  |+| False = True2
False |+| True  = True
_     |+| _     = False

---------------------------------------------------------------------------

-- Card: a data type containing a Rank and a Suit
-- 

-- Typical in Haskell to use the same name for the constructor as for thed data type
-- Card on LHS is the datatype, and Card on RHS is just a constructor taking two arguments
-- :t tells the type of a *data constructor* so 
-- :t Card :: Rank -> Suit -> Card (a function)
data Card = Card Rank Suit
    deriving (Show, Eq)

rank (Card r _) = r
suit (Card _ s) = s

-- Alternative way of writing
data Card' = Card' {rank'::Rank, suit':: Suit}

exCard1 = Card Ace Hearts
exCard2 = Card Jack Spades

---------------------------------------------------------------------------

cardBeats :: Card -> Card -> Bool

cardBeats' c1 c2 = suit c1 == suit c2 
                && rank c1 `rankBeats`rank c2

cardBeats (Card r1 s1) (Card r2 s2) = s1 == s2 && r1 `rankBeats`r2

---------------------------------------------------------------------------
-- Need to model a hand of cards
-- 0 or more cards
-- We could use a list for this!

-- Add is a constructor (function) which takes two arguments
-- Equality doesn't make sense, because that means if we shuffle the hand we would "have another hand"
data Hand = Empty | Add Card Hand
    deriving Show

exHand1 = Add (Card Ace Hearts) Empty
exHand2 = Add (Card Ace Spades) exHand1

---------------------------------------------------------------------------

handBeats :: Hand -> Card -> Bool

-- A recursive definition. To do things with a recursive data type,
-- we need to define recursive functions
handBeats Empty      c = False
handBeats (Add c' h) c = c' `cardBeats`c -- Either the first card beats c
                       || handBeats h c -- Or maybe the rest of the hand can

---------------------------------------------------------------------------

-- Choose a card to play
-- Even though we can't beat the card on the table, if I have a card of the same
-- suit, then I have to play it.
-- If I don't have a card with the matching suit, I want to play my worst card

chooseCard :: Card -> Hand -> Card 

chooseCard beat (Add c Empty) = c -- if we only have one card we have to play it
chooseCard beat (Add c rest)
    | bnb c  c' = c
    | bnb c' c  = c'
    | tnt c  c' = c
    | tnt c' c  = c'
    | otherwise = rankMin c c'
    where
        -- Trump-non-trumps: first card has same suit, second does not
        bnb c1 c2 = c1 `cardBeats` beat && not (c2 `cardBeats` beat)
        tnt c1 c2 = suit c1 == suit beat && suit c2 /= suit beat 

        c' = chooseCard beat rest
        
        rankMin c1 c2
            | rankBeats (rank c1) (rank c2) = c2
            | otherwise                     = c1





















