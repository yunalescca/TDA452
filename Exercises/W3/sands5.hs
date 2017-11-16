import Data.List
import Data.Char
import System.Random

-----------------------------------------------------------------------------

-- | the do-block builds something of type IO
-- Inside the do, you can use the special syntax <-, 
-- which will in a way convert the IO String to a String,
-- but since the do-block will always give back something¨
-- of type IO, then eventally the String will be converted
-- back into IO String
-- We're getting the string out locally, and we can only
-- use this "string" inside the do-block

-- var <- expr
-- var :: a    expr :: IO a 

copyFile :: FilePath -> FilePath -> IO()
copyFile fromFile toFile = 
    do contents <- readFile fromFile
       writeFile toFile contents

-----------------------------------------------------------------------------

wordFile = "words.txt"

-- find longest word in ...
longest :: FilePath -> IO String
longest file = 
    do wlist <- readFile file
       return (long wlist) -- the last line must produce IO String

    where long :: String -> String
          long = snd 
               . maximum
               . map (\w -> (length w, w))
               . words

-- * return in this context will take a String and turn
-- it into an IO String

-- * maximum ["aaa", "bb"] == "bb" because it's in 
-- alphabetical order

-----------------------------------------------------------------------------

dotwice :: IO a -> IO (a,a)
dotwice instr =
    do a1 <- instr
       a2 <- instr
       return (a1, a2)


-- | This won't run the instruction
dont :: IO a -> IO()
dont instr = return ()

test :: IO Int
test = 
    do ans <- return 42 -- trivial computation, nothing to do with control flow
       return 0 -- return not control flow construct

-----------------------------------------------------------------------------

-- sequence_

mySequence_ :: [IO a] -> IO()
mySequence_ []     = return ()
mySequence_ (i:is) = 
    do i 
       mySequence_ is

-- i >> mySequence_ is (equivalent)

-- sequence
mySequence :: [IO a] -> IO [a]
mySequence [] = return []
mySequence (i:is) = 
    do a  <- i
       as <- mySequence is 
       return (a:as)

-----------------------------------------------------------------------------

-- | EXERCISE : define the following instructions: 
copyAll :: [FilePath] -> FilePath -> IO()
copyAll fromFiles toFile = undefined
-- hint: sequence :: [IO a] -> IO[a]
--       map :: (a -> b) -> [a] -> [b]


-- | EXERCISE 
forLoop :: [a] -> (a -> IO()) -> IO()
forLoop = undefined
-- e.g. forLoop [1..10] print
-- will print numbers from 1 to 10

-----------------------------------------------------------------------------

guessLimit = 10

main :: IO()
main = 
    do w <- randomWord 
       gameLoop w "" -- guesses so far is the empty string


randomWord :: IO String
randomWord = 
    do wlist <- readFile wordFile
       let ws = words wlist
       n <- randomRIO (0, (length ws) - 1)
       return (ws !! n)



gameLoop :: String -> String -> IO()
gameLoop word guesses 
    | win       = showWin
    | lose      = showLose
    | otherwise = 
          do displayStatus
             g <- getLine
             gameLoop word (guesses ++ g)

    where win      = and [c `elem` guesses| c <- word]
          lose     = False
          showWin  = putStr "Win!" ++ word
          showLose = undefined
          displayStatus = putStrLn "type stuff"
















































