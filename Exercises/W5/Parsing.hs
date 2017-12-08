-- lecture notes

{-

Will export an abstract datatype for Parser, which means a 
datatype where you don't get to see what's inside it (how
it's built)

A function parse to user the Parser

And then a bunch of building blocks (combinators), a way of
building basic parser and more complicated parsers from simpler
parsers. Combinators = building parser from simpler parsers

-}


-- Parsing library
module Parsing
 ( Parser -- exports the type name but not the constructors
  ,parse,
  success,failure,sat,pmap,item,char,digit,
  (+++),(<:>),(>*>),(>->),(<-<),
  oneOrMore,zeroOrMore,chain
 ) 

where
import Data.Char
import Control.Applicative
import Control.Monad(liftM, ap)

{-}
instance Monad RefactoredParser where
    (>>=)  = (>*>) 
    return = success
-}

{----------------------
Week 4B part II

Aim: reusable Parser combinators including 
 a new type for the Parser, 
 but no export of the constructor
----------------------}

-- The same as it was before (type Parser = ...)
-- But now we have built a new type, which is exactly how it
-- was before, except now it is hidden inside a constructor P
-- When we only have one parameter, then by convention we call it
-- a newtype
-- Used by the function parse 
newtype Parser a = P (String -> Maybe (a,String))


-- Before the Parser was just a function, so when we wanted to
-- use the parser, we took the function and applied to a string.
-- Now that it's living inside a data type we can't do it as
-- directly anymore, so we define a helper function called parse

-- * Takes a Parser and a String, and applies the parser to the string
-- Our library is exporting the type Parser, but not exporting P
-- So the user of the library won't be able to grab the functiob from inside,
-- the only thing they can use is the parse function
parse :: Parser a -> String -> Maybe (a, String)
parse (P func) s = func s -- s can be omitted


-- * When you apply the parser to a string, it always
-- gives you Nothing
-- a constructor P containing a Nothing, always producing Nothing
-- P is the constructor, taking a function: It's a function that takes
-- a string, and always produces/returns nothing.
failure :: Parser a -- always fails
failure = P (\s -> Nothing) 


-- Doesn't care what a is, always returns it and the string
-- We will apply success over a string s
-- parse (success 42) "apa" --- applies (success 42) over "apa"
success :: a -> Parser a
success a = P (\s -> Just(a, s))

-----------------------------------------------------------------------------

-- * Parse a single character
item :: Parser Char
item = P $ \s ->
       case s of -- if the string contains at least one character, parse that character
           (c:s') -> Just (c, s')
           _      -> Nothing


-----------------------------------------------------------------------------

-- * parse either using p or else using q
-- First try p, if you fail then try q
-- It's a parser (P) which contains a function, and when
-- you give it a string it's gonna case analysis on it

-- parse (failure +++ success '!') "123" === Just ('!', "123")
infixr 5 +++
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P $ \s ->
          case (parse p s) of -- runs the function parse p s
              Just (a, s') -> Just (a, s') -- if the result was Just (a, s')
              Nothing      -> parse q s -- if the result was Nothing

-----------------------------------------------------------------------------

-- *  First we parse something using p, then depending on the result
-- it determines how we parse the second thing

-- p >*> f    parse using p to produce a, then parse using f a
-- Takes in a a parser and a function which returns a parser
infixl 1 >*>
-- Parses an a, than takes the result and feeds into a function
(>*>) :: Parser a -> (a -> Parser b) -> Parser b
p >*> f = P $ \s -> 
            case parse p s of 
                Just (a, s') -> parse (f a) s' -- if first parse was successful, then parse using
                                              -- (f a) on s
                Nothing      -> Nothing -- if the first thing fails, the whole thing fails

-----------------------------------------------------------------------------

-- sat p parse a single character satisfying property p
-- sat = satisfy!
sat :: (Char -> Bool) -> Parser Char
sat p = item >*> \c -> if p c then success c else failure

-- if c is the first symbol in string
char :: Char -> Parser Char
char c = sat (==c)

-- This is a parser, we need to use the parse function to use it
digit :: Parser Char
digit  = sat isDigit

-- * We want to parse an item, and then feed that into the function
-- which says ...
-- "aBxxx" == Nothing
-- "bBxxx" == Just (B, "xxx")
-- first item, then the function on the result
-- "hHej" -- then the result from item is Just ('h', "Hej")
-- and \c will be 'h', which is the first result 
lowerUpper = item >*> \c -> char (toUpper c)

-----------------------------------------------------------------------------


-- * pmap modifies the result of a parser
-- the result is called a 
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = p >*> \a -> success (f a)


-----------------------------------------------------------------------------


-- Parse p, throw away the result, then parse the rest with q
-- parse (char '{' >-> digit) "{123}"  == Just ('1', "23}")
-- parse (char '{' >-> digit) "123}"   == Nothing
-- parse (char '{' >-> digit) "{.123}" == Nothing

(>->) :: Parser a -> Parser b -> Parser b
p >-> q = p >*> \_ -> q --- so the \_ is the result from p


-- Parses a p, and then just parses q just to make sure it's there,
-- but throws it away
-- Parse with p, feed the result of that parse to the function
-- which parses with q and then throws the result of that away
-- and then succeeds with what you found in the first parser
(<-<) :: Parser b -> Parser a -> Parser b
p <-< q = p >*> \a -> q >-> success a


-- Parser a parses one thing
-- Parser [a] parses a bunch of things
-- First we parse the first thing, then the bunch of things
-- then return the result of joining the first thing with the rest
(<:>) :: Parser a -> Parser [a] -> Parser [a]
p <:> q = -- p >*> \a -> pmap (a:) 
    do a <- p
       as <- q
       return $ a:as 


-----------------------------------------------------------------------------


oneOrMore, zeroOrMore :: Parser a -> Parser [a]

-- you can always parse 0 or more of something
zeroOrMore p = oneOrMore p +++ success []
oneOrMore  p = p <:> zeroOrMore p


-----------------------------------------------------------------------------

-- Parse a list of as which are separated by bs
-- chain of p's separated by q's
chain :: Parser a -> Parser b -> Parser [a]
chain p q = p <:> zeroOrMore (q >-> p)

-- example: comma separated digits "1,2,3"
digitList = chain digit (char ',')





-- digit >*> \d -> sat (>d)
{- Parse a digit, and then parse a second digit which is 
bigger than the first digit. We are gonna parse two digits
but the second digit has to be bigger than the first digit.

Parse a *digit*, and then feeding the result to the following
function *\d* which takes a digit as its argument and then asks
the question, can we build something which satisfies the
property ">d"?

Two consecutive digits -}


-----------------------------------------------------------------------------

instance Functor Parser where
    fmap f = fmap f

instance Applicative Parser where
    pure  = pure
    (<*>) = (<*>)

instance Monad Parser where
    (>>=)  = (>*>)
    return = success

-----------------------------------------------------------------------------























