-- Defining our own data types

{-

Left, Right :: Either
Nothing :: Maybe
Just

map :: (a -> b) -> [a] -> [b]

show :: Show a => a -> String   (a has to be a part of the Show class, 
and then it will turn into a string) (so not generic)

(==) :: Eq a => a -> a -> Bool   (to use equality)

-}

-- type is Suit, constructors are Spades, Hearts, ... (constructors of a type)
data Suit = Spades | Hearts | Diamonds | Clubs -- enumerated types
            deriving (Show, Eq)

data Colour = Black | Red
              deriving (Show, Eq)

colour :: Suit -> Colour -- This a function! 
colour Spades   = Black
colour Clubs    = Black -- if overlapping then must appear in correct order
colour _        = Red -- _ is a *wild card* you are not going to use the value of the argument
-- colour Hearts = Red

-- Cards have ranks : 2, 3, .. 10, Jack, Queen, King, Ace

data Rank = Numeric Int | Jack | Queen | King | Ace
            deriving (Show, Eq, Ord) -- To have Ord we need Eq

-- data Rank' = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | Jack | Queen | King | Ace

rankBeats :: Rank -> Rank -> Bool 
{-rankBeats _ Ace   = False -- Nothing beats an ace
rankBeats Ace _   = True 
rankBeats _ King  = False
rankBeats King _  = True
rankBeats _ Queen = False
rankBeats Queen _ = True
rankBeats _ Jack  = False
rankBeats Jack _  = True
rankBeats (Numeric m) (Numeric n) = m > n-}

rankBeats r1 r2 = r1 > r2   --(must have Ord, and then put the constructors in the right order)

------------------------------------------------------------------------

-- data keyword when you define things that didn't exist before
-- can use the type keyword for things that already exist (Rank and Suit)
type Card1 = (Rank, Suit) -- it's just another name for the tuple

data Card2 = Card2 Rank Suit deriving Show
{-rank :: Card -> Rank
rank (Card r s) = r

suit :: Card -> Suit
suit (Card r s) = s-}

data Card = Card {rank::Rank, suit::Suit} deriving Show


example_card_1 = Card Ace Spades
example_card_2 = Card {rank = King, suit = Hearts}

-- A Card beats another card when it has the same suit and it beats
-- the rank of the other card

cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) = s1 == s2 && rankBeats r1 r2

-- Alternative definition 
cardBeats' card1 card2 = suit card1 == suit card2
                       && rankBeats (rank card1) (rank card2)




data Hand = Empty | Add Card Hand -- One card and the rest of the hand
            deriving Show

example_hand_0 = Empty
example_hand_1 = Add example_card_1 Empty
example_hand_2 = Add example_card_2 example_hand_1 


-- An empty hand beats nothing. A non-empty hand can beat a card 
-- if the first card can, or if the rest of the hand can
handBeats :: Hand -> Card -> Bool

handBeats Empty beat = False
handBeats (Add card hand) beat = cardBeats card beat || handBeats hand beat


-- Given a card to beat and a hand, choose a card from the hand that can
-- beat the card to beat, if possible.
-- Choose the lowest card that beats the card to beat
-- If you can follow suit, choose the lowest card of the same suit
-- Otherwise, choose the lowest card
chooseCard :: Card -> Hand -> Card

chooseCard beat hand 
    -- if We have a card on the hand that can beat the Card
    | handBeats hand beat = lowestCard (betterCards hand beat) -- pick the lowest card of the cards in betterCards hand
    | haveSuit hand (suit beat) = lowestCard (sameSuit hand (suit beat)) -- Else follow suit, if possible 
    | otherwise = lowestCard hand -- else just lowest card


-- * If there is a card in our hand with the suit of Suit
haveSuit :: Hand -> Suit -> Bool
haveSuit Empty s     = False
haveSuit (Add c h) s = suit c == s || haveSuit h s 


-- * Find the lowest card on our hand
lowestCard :: Hand -> Card
lowestCard (Add c Empty) = c --lowestCard Empty = -- not the base case
lowestCard (Add c h) 
    | rank c < rank low  = c
    | otherwise          = low 
    
    where 
        low = lowestCard h -- remaining hand


-- * Add all cards to the hand which will beat the card Card (beat)
betterCards :: Hand -> Card -> Hand
betterCards Empty beat     = Empty
betterCards (Add c h) beat 
    | cardBeats c beat     = Add c (betterCards h beat)
    | otherwise            = betterCards h beat


-- * Returns a hand with cards of the same suit as Suit
sameSuit :: Hand -> Suit -> Hand
sameSuit Empty s   = Empty
sameSuit (Add c h) s 
    | suit c ==  s = Add c (sameSuit h s)
    | otherwise    = sameSuit h s


























