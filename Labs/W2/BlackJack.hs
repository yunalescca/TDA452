module BlackJack where
import Cards
import RunGame
import System.Random
import Test.QuickCheck hiding (shuffle)

-- * Assignment 3.2

{-

size hand 2
	= size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
	= 1 + size (Add (Card Jack Spades) Empty)
	= 1 + 1 + size Empty
	= 1 + 1 + 0
	= 2

-}

-----------------------------------------------------------------------------

-- * Assignment 3.4

-- | Returns the empty hand
empty :: Hand
empty = Empty 


-- | Returns the value of the given rank
valueRank :: Rank -> Integer
valueRank (Numeric int) = int
valueRank Ace           = 11
valueRank _             = 10


-- | Returns the value of the given card
valueCard :: Card -> Integer
valueCard card = valueRank (rank card) 


-- | Calculates number of aces on given hand
numberOfAces :: Hand -> Integer
numberOfAces Empty  = 0
numberOfAces (Add c h) 
    | rank c == Ace = numberOfAces h + 1
    | otherwise     = numberOfAces h

exHand = Add (Card Jack Hearts) (Add (Card (Numeric 6) Hearts) (Add (Card Ace Hearts) Empty))

-- | Calculates the hand's value. If value exceeds 21 and 
-- there are aces, then a new value will be calculated 
-- where aces' worth are 1
value :: Hand -> Integer
value Empty = 0
value (Add c h) = 
    if totalValue > 21 && aces > 0
        then totalValue - 10 * aces
        else totalValue
    where
        totalValue = valueCard c + value h
        aces       = numberOfAces (Add c h)


-- | If value on hand for player exceeds 21, then the player is busted
gameOver :: Hand -> Bool
gameOver Empty = False
gameOver (Add c h) 
    | value (Add c h) > 21 = True
    | otherwise            = False


-- | The guest wins if guest is not busted, and if bank is busted OR
-- if bank is not busted and guest has higher value than the bank
winner :: Hand -> Hand -> Player
winner guest bank
    | not (gameOver guest) && ((gameOver bank 
        || not (gameOver bank) && value guest > value bank)) = Guest
    | otherwise = Bank


-- | Puts the first hand on top of the second hand
(<+) :: Hand -> Hand -> Hand
(<+) Empty hand2   = hand2
(Add c h) <+ hand2 = Add c (h <+ hand2)


-- | Returns the full deck (52 card)
fullDeck :: Hand 
fullDeck = deck Hearts <+ deck Spades <+ deck Diamonds <+ deck Clubs


-- | Given a suit, returns all 13 cards in that suit
deck :: Suit -> Hand
deck suit = Add (Card (Numeric 2)  suit)
           (Add (Card (Numeric 3)  suit)
           (Add (Card (Numeric 4)  suit)
           (Add (Card (Numeric 5)  suit)
           (Add (Card (Numeric 6)  suit)
           (Add (Card (Numeric 7)  suit)
           (Add (Card (Numeric 8)  suit)
           (Add (Card (Numeric 9)  suit)
           (Add (Card (Numeric 10) suit)
           (Add (Card Jack         suit)
           (Add (Card Queen        suit)
           (Add (Card King         suit)
           (Add (Card Ace          suit) 
            Empty))))))))))))


-- | Given a deck and a hand, draw a card from the deck and place in the hand
-- Returns the rest of the deck and the new hand as a pair
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _           = error "draw: the deck is empty"
draw (Add c deck) hand = (deck, (Add c hand))


-- | Returns the bank's hand with 16 or higher as value
playBank :: Hand -> Hand
playBank deck = snd (playBank' deck Empty)


-- | Helper function to calculate the value of the bank's hand
playBank' :: Hand -> Hand -> (Hand, Hand)
playBank' deck bankHand 
    | value bankHand < 16 = playBank' deck' (bankHand')
    | otherwise = (deck', bankHand)

    where (deck', bankHand') = draw deck bankHand


-- | Shuffles a deck 
shuffle :: StdGen -> Hand -> Hand 
shuffle g hand 
    | size hand /= 0 = Add (getCard index hand) 
                       $ shuffle g (removeCard index hand)
    | otherwise      = Empty
        where index  = oneRandomInteger g (size hand)


-- | Helper function: gets the n:th card from the deck
getCard :: Integer -> Hand -> Card
getCard 1 (Add c _) = c
getCard n (Add c h) = getCard (n - 1) h


-- | Helper function: removes the n:th card from the deck
removeCard :: Integer -> Hand -> Hand
removeCard 1 (Add _ h) = h
removeCard nthCard (Add c h) = Add c (removeCard (nthCard - 1) h)
    

-- | Generates a random integer
oneRandomInteger :: StdGen -> Integer -> Integer
oneRandomInteger g sizeDeck  = n1
    where (n1, g1)           = randomR (1, sizeDeck) g


-----------------------------------------------------------------------------

-- | PROPERTIES 

-- | Checks if <+) is associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3


-- | Checks if size of combined hands is the same as size of the 
-- two individual hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = 
    size (p1 <+ p2) == size p1 + size p2


-- | Checks if a card is in a deck before it has been shuffled,
-- then it should be in the deck afterwards as well
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

-- Helper function: Checks if card is in a hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h


-- | Checks if the size of the hans is the same before
-- and afterwards it has been shuffled
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h =
    size h == size (shuffle g h)

-----------------------------------------------------------------------------

implementation = Interface
  { iEmpty     = empty
  , iFullDeck  = fullDeck
  , iValue     = value
  , iGameOver  = gameOver
  , iWinner    = winner 
  , iDraw      = draw
  , iPlayBank  = playBank
  , iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation






























