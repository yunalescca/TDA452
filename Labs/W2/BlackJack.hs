module BlackJack where
import Cards
import RunGame
import System.Random

-- * Assignment 3.2

{-

size hand 2
	= size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
	= 1 + size (Add (Card Jack Spades) Empty)
	= 1 + 1 + size Empty
	= 1 + 1 + 0
	= 2

-}

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
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _           = error "draw: the deck is empty"
draw (Add c deck) hand = (deck, (Add c hand))


-- |
playBank :: Hand -> Hand
playBank deck = snd (playBank' deck Empty)


-- |
playBank' :: Hand -> Hand -> (Hand, Hand)
playBank' deck bankHand 
    | value bankHand < 16 = playBank' deck' (bankHand')
    | otherwise = (deck', bankHand)

    where (deck', bankHand') = draw deck bankHand


-- |
{-shuffle :: StdGen -> Hand -> Hand 
shuffle g hand = 
    where (card', oldHand) = removeCard oneRanomInteger g hand-}

-- | 
removeCard :: Integer -> Hand -> Hand
removeCard _ Empty = Empty
removeCard nthCard (Add c h)
    | nthCard == 1 = h
    | otherwise    = Add c (removeCard (nthCard - 1) h)
    

-- |
oneRanomInteger :: StdGen -> Integer -> Integer
oneRanomInteger g sizeDeck  = n1
    where (n1, g1)          = randomR (1, sizeDeck) g























