module BlackJack where
import Cards
import RunGame


-- * Assignment 3.2

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


-- | 
numberOfAces :: Hand -> Integer
numberOfAces Empty  = 0
numberOfAces (Add c h) 
    | rank c == Ace = 1 + numberOfAces h
    | otherwise     = 0 + numberOfAces h


-- | 
value :: Hand -> Integer
value = undefined


-- | 
gameOver :: Hand -> Bool
gameOver = undefined


-- | 
winner :: Hand -> Hand -> Player
winner = undefined 






















