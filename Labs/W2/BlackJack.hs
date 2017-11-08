module BlackJack where
import Cards
import RunGame

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
    | rank c == Ace = 1 + numberOfAces h
    | otherwise     = 0 + numberOfAces h


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
    | gameOver guest == False && ((gameOver bank == True) 
        || (gameOver bank == False && value guest > value bank)) = Guest
    | otherwise = Bank


























