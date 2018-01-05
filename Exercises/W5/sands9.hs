-- * MONADS

-----------------------------------------------------------------------------
module Sands9 where

import Test.QuickCheck
import Data.List
import Data.Char
import Parsing


-- if doTwice (elements ['a'..'z']) then we need sample to print it, because
-- elements :: [a] -> Gen a, and there's no instance of show
-- b is a Monad / b is an instruction, and it will run that instruction
-- twice. 

-- doTwice getLine doesn't need print, because getLine :: IO String
-- produces the result it got from running the instruction getLine twice
doTwice :: Monad a => a b -> a (b,b)
doTwice i = 
    do a <- i
       b <- i
       return (a,b)


doTwice' i = 
    i >>= \a -> 
    i >>= \b -> 
    return (a,b)

-----------------------------------------------------------------------------

blist = 
    do char '{'
       ds <- chain digit (char ',')
       char '}'
       return $ map digitToInt ds

-- == 

-- >> is the same as >->

blist1 = 
    char '{' >> do ds <- chain digit (char ',')
                   char '}'
                   return $ map digitToInt ds


blist2 = 
    char '{' >> 
    chain digit (char ',') >>= \ds -> 
    do char '}'
       return $ map digitToInt ds


blist3 = 
    char '{' >> 
    chain digit (char ',') >>= \ds -> 
    char '}' >>
    do return $ map digitToInt ds


blist4 = 
    char '{' >> 
    chain digit (char ',') >>= \ds -> 
    char '}' >>
    return (map digitToInt ds) -- remove $

-----------------------------------------------------------------------------

prop_blist =
    parse blist "{1,2,3,4}xxx" == Just ([1,2,3,4], "xxx")


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- EXAMPLE OF ANOTHER MONAD: MAYBE
-----------------------------------------------------------------------------

type CarReg = String ; type PNr     = String
type Name   = String ; type Address = String

carRegister :: [(CarReg, PNr)]
carRegister = 
    [("FYN 433", "850219-1234"),
     ("GYN 434", "850219-1234"),
     ("JBD 007", "750408-0909")]


nameRegister :: [(PNr, Name)]
nameRegister =
    [("750408-0909", "Dave"),
     ("850219-1234", "Bob") ,
     ("890929-C234", "Pierre")]



addressRegister :: [((Name, PNr), Address)]
addressRegister = 
    [(("Dave", "750408-0909"), "42 Streetgatan\n Askim"),
     (("Bob" , "850219-1234"), "1 Chalmers Av\n Gothenburg")]
    

-----------------------------------------------------------------------------

-- Given a registration number, 
-- return the name and address of owner, if defined
-- If we don't find it in the table, we get Nothing

-- lookup :: Eq a => a -> [(a,b)] -> Maybe b

-- a bit ugly looking, partly because we at each step of the computation,
-- has to check if we get Nothing or something else.
-- In a language with exceptions this would be hidden away using exceptions,
-- you'd just to a sequence of lookups and then catch the exception
-- when the lookup fails (tidier code).
-- Haskell doesn't have exceptions, we have to program them ourselves
billingAddress :: CarReg -> Maybe (Name, Address)
billingAddress car = 
    case lookup car carRegister of
        Nothing -> Nothing
        Just pnr -> case lookup pnr nameRegister of
                        Nothing -> Nothing
                        Just name -> case lookup (name, pnr) addressRegister of
                                         Nothing -> Nothing
                                         Just addr -> Just (name, addr)


-- Redefined in Monadic style:

billingAddress' :: CarReg -> Maybe (Name, Address)
billingAddress' car = 
    do pnr  <- lookup car carRegister
       name <- lookup pnr nameRegister
       addr <- lookup (name, pnr) addressRegister
       return (name, addr)


-- getting rid of the do-blocks
-- from lecture notes: Just x >>= f = f x
-- So *lookup car carRegister* will produce Just pnr, and we feed that
-- to the function f (which is the rest of the instructions)
-- And if we get Nothing, we will give back Nothing
billingAddress'' :: CarReg -> Maybe (Name, Address)
billingAddress'' car = 
    lookup car carRegister >>= \pnr -> 
    lookup pnr nameRegister >>= \name -> 
    lookup (name, pnr) addressRegister >>= \addr ->
    return (name, addr)


-----------------------------------------------------------------------------


-- What type does test' have? 
-- Nothing gives us a hint that it's something with Maybe of something
-- A Maybe of some type of number
-- test' :: Maybe Integer (chooses the simplest type)
-- we can run it just as test', because it's something in the show class

-- It returns Nothing - why?
test' = 
    do return 32
       x <- Nothing
       return 42 
     

test'' =
    return 32 >>= \_ ->  -- Because it throws the return value away, we can see
                         -- that it definitely doesn't return 32
    Nothing >>= \x ->    -- Nothing is fed into the function (\x -> return 42)
                         -- According to the function: Nothing to the left of anything
                         -- returns Nothing, and therefore it will return 
                         -- Nothing for the whole thing
    return 42
                         -- the only thing that *return* does is taking a 
                         -- value and lifting it into a monad




-- when the pattern matching fails in a monadic operation it will call 
-- the Fail function and fail does depends on the monad

-- In the Maybe Monad, the fail operation gives you Nothing, it doesn't crash

test3 = 
    do putStrLn "hello"
       "42" <- getLine -- pattern matching
       putStrLn "World"


-----------------------------------------------------------------------------


















