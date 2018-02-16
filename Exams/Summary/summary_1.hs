
import Test.QuickCheck
import Data.List(nub)

-- | DATATYPES

{-
	* Rank is a new data type, which consists of constructors (definitions) on the RHS
	* Numeric, Jack, Queen, King and Ace are constructors. 
	  Constructor MAY take an argument. In this example, Numeric takes and argument, which
	  is an Int. The rest take no arguments. 
	* The constructors can be seen as functions. We can use :t on functions.
	* :t Numeric :: Int -> Rank
	  :t Jack    :: Rank
	  That is, a Numeric is a function which takes an Int and returns a Rank
	  A Jack simply returns a Rank
	* :t Numeric 5 :: Rank 
-}

data Rank = Numeric Int | Jack | Queen | King | Ace
    deriving (Eq, Ord)


data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Eq)


-- * Define the functions rank and suit within the data type. They simply return 
-- a Rank and Suit, respectively.
data Card = Card {rank :: Rank, suit :: Suit} 
    deriving (Eq)

instance Show Rank where
    show (Numeric n) = show n
    show Jack        = "J"
    show Queen       = "Q"
    show King        = "K"
    show Ace         = "A"

instance Show Suit where
    show Spades   = "♠"
    show Hearts   = "♥"
    show Diamonds = "♦"
    show Clubs    = "♣"

instance Show Card where 
    show (Card r s) = "[" ++ show r ++ show s ++ "]"

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- | HIGHER-ORDER FUNCTIONS 

{-
	* folds take an operator, a base case and a list. 
	* foldl will perform the operation on the base case and the head of the list 
	  This will result in a new "base case" (accumulator). Next, it performs the 
	  operation between the accumulator and the second element of the list. 
	  Moves from left to right.
	* foldr Moves from right to left
	* When building up a new list with the help of fold, we typically use foldr, 
	  because the : operator is cheaper than the ++ operator. 
	  foldl uses ++ , whereas foldr uses :
	* The difference between foldl and foldr lies in if the function is associative or not
	  foldl (+) and foldr (+) would produce the same result, because + is associative
	  Whereas foldl (-) and foldr (-) would not, because 1-10 is not the same as 10-1
	* foldr more efficient for lazy evaluation. Works on infinite lists because it always
	  moves to the front of the list. 
	* foldl won't compute anything until the final result is required, i.e. it has to iterate
	  through the entire list! 
	* foldl rarely the better choice, can cause stack overflow! Usually worth in that case 
	  to use foldl', because it's strict and stops you building up a long list of 
	  intermediate results. That is, foldl' is the opposite to lazy evaluation, because each 
	  function application is evaluated along the way.


	* Lambda functions can take any amount of arguments
	  (\x y -> ...)
	  map (\x -> x + 1) [1..10] == map (+1) [1..10]
	  filter (\x -> even x) [1..10] == [2,4,6,8,10]
	* Instead of local definitions


	* lines breaks a string into a list of strings. Separated where a \n is encountered
	* unlines break a list of strings into a string. Adds \n between each element

	* words breaks a string into a list of strings. Separated by space character.
	* unwords breaks a list of strings into a string. Adds space character between.
	
	* takeWhile takes from a list as long as a predicate is true
	* dropWhile drops from a list as long as a predicate is true

-}

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- | IO AND DATA GENERATORS

{-
	* IO is a Monad = an instruction
	* IO is the instructions for interacting with the operative system

	* Reading input from the user can be done in a do block
	* Can use getLine to get a line from the user
	* Use putStr/Ln to write strings
	* Use print to write other things
	  or just use show on the other thing


	* arbitrary :: Arbitrary a => Gen a
	* Gen is also a monad, meaning we can return something of type Gen a 
	  in a do block. 
	* Because it's a monad, it means that x <- g :: Gen a, will pick out the a,
	  and save it in x. If g :: Gen Int, then x :: Int
	* Some generators
	  elements  :: [a]            -> Gen a
	  choose    :: (a, a)         -> Gen a
	  oneOf     :: [Gen a]        -> Gen a
	  frequency :: [(Int, Gen a)] -> Gen a
	* n <- arbitrary :: Gen Integer
	  will generate a random Integer
	  n <- arbitrary :: Gen Rank
	  would generate an arbitrary Rank
	* For something to be able to be Generates, it needs to be an instance of Arbitrary
-}

instance Arbitrary Rank where
    arbitrary = frequency [(4, rRoyal), (9, rNumeric)]

        where rRoyal   = elements [Jack, Queen, King, Ace]
              rNumeric = fmap Numeric $ choose (2, 10)


instance Arbitrary Suit where
    arbitrary = elements [Hearts, Spades, Diamonds, Clubs]


-- sample (arbitrary :: Gen Card)
instance Arbitrary Card where
    arbitrary = do r <- arbitrary 
                   s <- arbitrary
                   return $ Card r s


----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- | RECURSIVE DATA TYPES AND PARSING EXPRESSIONS 

{-
	* Most of the recursive data types that we define in Haskell are tree-shaped,
	  not list-shaped.
-}

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- | MAYBE

{-
	* data Maybe a = Nothing | Just a
 	* This can be viewed as a present, which is either empty (Nothing)
 	  or contains a value and context (Just a)
 	* Just = value ; a = context
-}

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------


-- | FUNCTORS AND APPLICATIVE FUNCTORS

{-
	* Functors are things that can be mapped over
	* instance Functor [] where
	      fmap = map
	* instance Functor Maybe where
	      fmap f Nothing  = Nothing
	      fmap f (Just a) = Just (f a)
	* It is possible to either write the definition directly, as with Maybes,
	  or direct to another function, as with []

	* Applicative is an extended version of functors. So classes that wish to be
	  applicative need to be functors as first.
	* class Functor f => Applicative f where
	      pure  :: a -> f a
	      (<*>) :: f (a -> b) f a -> f b 
	* (<*>) looks similar to fmap. But where fmap takes a function, a functor and then 
	  applies that function to the functor, <*> instead takes a function wrapped inside 
	  a functor, another functor, "extracts" the function and then maps it over the 
	  other functor

	* These are equivalent
	  fmap  (+1) [1..10]
	  (+1)   <$> [1..10]
	  [(+1)] <*> [1..10] -- function (+1) wrapped in Functor []

	  fmap       (+1)  (Just n)
	  (+1)        <$>  (Just n)
	  (Just (+1)) <*>  (Just n)

    * func <$> x <*> y <*> z ... 
 -}

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- | PRELUDE FUNCTIONS

{-
	* span  : returns the tuple of (takeWhile, dropWhile)
	* break : returns the tuple of (dropWhile, takeWhile)
	
	* any   : takes a predicate and a list, checks if any element in list satisfies p
	* all   : takes a predicate and a list, checks if all elements satisfies p
	
	* isInfixOf  : searches for sublist 
	* isPrefixOf : searches for prefix
	* isSuffixOf : searches for suffix
	
	* partition   : takes a predicate and a list, divides the list into ([p x], [not p x])
	
	* find        : searches the list for elements which satisfies a predicate. Returns 
	  the first element as a Maybe.
	* findIndex   : searches list for an element. Returns a Maybe with index of first hit
	* findIndices : returns a list of ints with all indices of elements that satisifies p
	* elemIndex   : finds the index of an element in a list. Returns a Maybe
	* elemIndices : finds the indices of an element in a list. Returns a list of ints
-}

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- | MONADS

{-
	* The truth about Do: Do syntax is just shorthand (syntactic sugar).
	* do act1
	     act2
	  ==
	  act1 >> act2
	  ==
	  act1 >>= \_ -> act2
	  
	  Which essentially means, do action 1, throw away the result and do action 2.
	  Save the result from act1, and parse it into the function \_ -> act2. The 
	  function, however, doesn't care what the result is, so it simply moves on and
	  does action 2.

	* do r <- act2
	     act2 r
	  == 
	  act1 >>= (\r -> act2 r)
	  Save the result of doing action 1, and use that when running act 2
 
 	* do action
 	  == 
 	  action


 	* Maybes are of the Monad class
 	  instance Monad Maybe where
 	      (Just x) >>= k     = k x
 	      Nothing  >>= k     = Nothing
	* If we just have (Just x), that that x and feed it to the function k
	
-}

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------














