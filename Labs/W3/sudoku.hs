 module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Char
import Data.Maybe(fromJust, listToMaybe)

-----------------------------------------------------------------------------

-- | Representation of sudoku puzzlese (allows some junk)
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving (Show,Eq)  

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing



-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sudoku = 
  correctRowSize sudoku && correctColumnSize sudoku && validElement sudoku

-- | Checks is number of rows is correct
correctRowSize :: Sudoku -> Bool
correctRowSize (Sudoku rs) = length rs == 9

-- | Checks if number of columns is correct
correctColumnSize :: Sudoku -> Bool
correctColumnSize (Sudoku rs) = length (transpose rs) == 9


-- | Checks if the sudoku only contains valid elements
validElement :: Sudoku -> Bool
validElement (Sudoku rs) = and $ map (\x -> x `elem` ms) $ concat rs
    
    where ms = [Nothing, Just 1, Just 2, Just 3, Just 4, Just 5,
                Just 6, Just 7, Just 8, Just 9]



-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rs) = and $ map (\x -> x /= Nothing) $ concat rs

-----------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rs) = 
    putStrLn $
    unlines [ [ if element == Nothing 
        then '.' 
        else intToDigit (fromJust element) | element <- r] 
        | r <- rs]  


-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do sudoku <- readFile file
                     let c = convertToSudoku sudoku
                     if isSudoku c
                         then return c
                         else error "Not a sudoku!"

-- | converts a string to a Sudoku
convertToSudoku :: String -> Sudoku 
convertToSudoku string = 
    Sudoku [ [if s == '.' 
        then Nothing 
        else Just (digitToInt s) | s <- ss ]| ss <- lines string ]

-----------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(1, number),(1, nothing)]
    where number  = do n <- choose (1,9)
                       return $ Just n
          nothing = do n <- elements [Nothing]
                       return n 


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
    arbitrary = do rows <- vectorOf 9 (vectorOf 9 cell)
                   return (Sudoku rows)


-- * C3
-- | Property for checking that all sudokus are valid
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku 

-----------------------------------------------------------------------------

-- * D1

-- | A Block is either 9 rows, 9 columns or 9 3x3 blocks
type Block = [Maybe Int]

-- | Checks so that a Block doesn't contain duplicate elements
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (b:bs)
    | b /= Nothing && b `elem` bs = False
    | otherwise = isOkayBlock bs


-- * D2

-- | Builds a list of all the different Blocks
blocks :: Sudoku -> [Block]
blocks (Sudoku rs) = rs ++ transpose rs ++ build3x3 rs 


-- | Builds the 3x3 blocks 
build3x3 :: [[Maybe Int]] -> [Block]
build3x3 [] = []
build3x3 rs = 
    [concat $ map (take 3) top3] ++ 
    [concat $ map (take 3) (map (drop 3) top3)] ++ 
    [concat $ map (drop 6) top3] ++
    build3x3 (drop 3 rs)

    where top3 = take 3 rs


-- | Checks the correct length of all blocks
prop_buildBlocks :: Sudoku -> Bool
prop_buildBlocks sudoku = 
    length (blocks sudoku) == 27 &&
    length rs              == 9  &&
    length (transpose rs)  == 9  &&
    length (build3x3 rs)   == 9 
     
    where rs = rows sudoku



-- * D3

-- | Checks so that no Block contains duplicate elements
isOkay :: Sudoku -> Bool
isOkay sudoku = and [isOkayBlock b | b <- (blocks sudoku)]

-----------------------------------------------------------------------------

-- * E1

type Pos = (Int, Int)

-- | Returns all the positions which are blank in a sudoku
blanks :: Sudoku -> [Pos]
blanks (Sudoku rs) = 
    [(x,y) | x <- [0..8], y <- [0..8], (rs !! x) !! y == Nothing]


-- | Checks so that a blank cell is actually blank
prop_blanks :: Sudoku -> Bool
prop_blanks (Sudoku rs) = and [(rs !! (fst ts)) !! (snd ts) == Nothing 
                                  | ts <- blanks (Sudoku rs)]



-- * E2
-- | Inserts an element into a list
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) [] _ = []
(!!=) (x:xs) (pos, e)
    | pos == 0  = e : xs
    | otherwise = x : (!!=) xs (pos - 1, e)


-- Checks so length is preserved 
prop_insertlength xs (pos, e) = 
    pos >= 0 && pos < length xs ==> length xs == length (xs !!= (pos, e))
    where types = e :: Integer


-- Checks so that the inserted element is in the list after !!=
prop_insertelem xs (pos, e) = 
    pos >= 0 && pos < length xs ==> e `elem` (xs !!= (pos, e))
    where types = e :: Integer



-- * E3
-- | Updates a sudoku with a value in a certain position
update :: Sudoku -> Pos -> Maybe Int -> Sudoku 
update (Sudoku (r:rs)) (0,y) value = Sudoku (r !!= (y, value) : rs)
update (Sudoku (r:rs)) (x,y) value =
    Sudoku(r : rows (update (Sudoku rs) (x-1, y) value))


-- | Creates a new datatype so QuickCheck won't give up on test.
newtype NewPos = P Pos
    deriving Show

-- | A NewPos can only be between (0,0) and (8,8)
instance Arbitrary NewPos where
    arbitrary = do x <- choose (0,8)
                   y <- choose (0,8)
                   return $ P (x,y)


-- Checks so that the sudoku was updated properly
prop_update sudoku (P (x,y)) value = 
    ((rows (update sudoku (x,y) value)) !! x) !! y == value



-- * E4
-- | Given a sudoku and position, returns all the numbers that could
-- be legally written in that cell
candidates :: Sudoku -> Pos -> [Int]
candidates (Sudoku rs) (x,y) = 
   map (fromJust) (((result \\ (rs !! x)) 
                            \\ ((transpose rs) !! y))
                            \\ (pickBlock rs (x,y)))
   where 
      result = [Just 1, Just 2, Just 3, Just 4, 
                Just 5, Just 6, Just 7, Just 8, Just 9]
    

-- Returns a 3x3 block
pickBlock rs (x,y)
    | x < 3 && y < 3 = (build3x3 rs) !! 0
    | x < 3 && y < 6 = (build3x3 rs) !! 1
    | x < 3 && y < 9 = (build3x3 rs) !! 2
    | otherwise = pickBlock (drop 3 rs) (x - 3, y)
    


-----------------------------------------------------------------------------

-- * F1
-- | Solves a given sudoku
solve :: Sudoku -> Maybe Sudoku
solve sudoku 
    | not (isSudoku sudoku || isOkay sudoku) = Nothing
    | otherwise = solve' sudoku


-- | Helper function of solve
solve' :: Sudoku -> Maybe Sudoku
solve' sudoku
    | blanks sudoku == [] = Just sudoku -- if no blanks left then we are done
    | otherwise = case updatedSud of 
                      Nothing -> Nothing
                      Just sud -> case solve sud of
                                      Nothing -> Nothing
                                      _       -> solve' sud 

    where emptyCell  = head (blanks sudoku) -- choose the first blank cell
          cands      = candidates sudoku emptyCell -- find all candidates

          updatedSud = possibleSolution sudoku emptyCell cands


-- | Helper function that checks if the position has a possible candidate, 
-- updates the position with the candidate
-- and then returns the updated sudoku
possibleSolution :: Sudoku -> Pos -> [Int] -> Maybe Sudoku
possibleSolution sudoku p cs = 
    case c of
        Nothing -> Nothing 
        Just x  -> case (solve $ u (Just x)) of
                       Nothing             -> possibleSolution sudoku p (drop 1 cs)
                       Just solution       -> Just solution
    where c = possibleCandidate cs
          u = update sudoku p


-- Gives the first candidate of a cell, from the list of all possible candidates 
possibleCandidate :: [Int] -> Maybe Int
possibleCandidate [] = Nothing
possibleCandidate (x:xs) = Just x



-- * F2
-- Reads a sudoku and solves it
readAndSolve :: FilePath -> IO ()
readAndSolve file = 
    readSudoku file >>= \sud -> 
    printSudoku (fromJust (solve sud))



-- * F3
-- Checks so that the first sudoku is a solution of the second one,
-- and so that all elements in the second is maintained in the first
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf first second = 
    ((solved1 /= Nothing) && isFilled first) 

    && maintained (concat (rows first)) (concat (rows second))

    where solved1 = solve first

-- Helper: checks so all elements in the second sudoku are maintained in the first one
maintained :: [Maybe Int] -> [Maybe Int] -> Bool
maintained [] [] = True
maintained (f:fs) (s:ss)
    | s == Nothing = (maintained fs ss)
    | otherwise = f == s && (maintained fs ss)





-- * F4
-- If we generate a solveable sudoku, then the solution of that should be a proper
-- solution of the sudoku we originally generated
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = solve sud /= Nothing ==> 
    case solve sud of
        Nothing       -> label "Not solveable" True
        Just solution -> label "Correct solution" $ isSolutionOf solution sud
    

fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 30 } prop

-----------------------------------------------------------------------------
























