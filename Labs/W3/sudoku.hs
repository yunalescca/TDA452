
import Test.QuickCheck
import Data.List
import Data.Char
import Data.Maybe(fromJust)

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
correctRowSize sudoku = length (rows sudoku) == 9

-- | Checks if number of columns is correct
correctColumnSize :: Sudoku -> Bool
correctColumnSize sudoku = 
  and $ map (\x -> (length x == 9)) (rows sudoku)

-- | Checks if the sudoku only contains valid elements
validElement :: Sudoku -> Bool
validElement sudoku = and $ map (\x -> x `elem` ms) $ concat $ rows sudoku
    where ms = [Nothing, Just 1, Just 2, Just 3, Just 4, Just 5,
                Just 6, Just 7, Just 8, Just 9]



-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled sudoku = and $ map (\x -> x /= Nothing) $ concat $ rows sudoku

-----------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = 
    putStrLn $
    unlines [ [ if element == Nothing 
        then '.' 
        else intToDigit (fromJust element) | element <- row] 
        | row <- rows sudoku]  


-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do sudoku <- readFile file
                     let c = convertToSudoku sudoku
                     if isSudoku c
                         then return c
                         else error "Not a sudoku!"


convertToSudoku :: String -> Sudoku 
convertToSudoku string = 
    Sudoku [ [if s == '.' 
        then Nothing 
        else Just (digitToInt s) | s <- ss ]| ss <- lines string ]

-----------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(1, number),(9, nothing)]
    where number  = do n <- choose (1,9)
                       return $ Just n
          nothing = do n <- elements [Nothing]
                       return n 


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-----------------------------------------------------------------------------




































example2 :: Sudoku
example2 =
    Sudoku
      [ [j 3,j 6,j 1,j 2,j 7,j 1,j 5,j 8,j 4]
      , [j 1,j 5,j 8,j 7,j 1,j 1,j 1,j 8,j 1]
      , [j 2,j 1,j 9,j 2,j 1,j 4,j 7,j 1,j 1]
      , [j 5,j 2,j 6,j 5,j 1,j 3,j 1,j 2,j 8]
      , [j 4,j 3,j 4,j 5,j 1,j 2,j 1,j 1,j 9]
      , [j 6,j 7,j 1,j 4,j 6,j 1,j 1,j 1,j 1]
      , [j 7,j 6,j 5,j 3,j 1,j 8,j 9,j 1,j 1]
      , [j 8,j 8,j 3,j 2,j 1,j 1,j 1,j 6,j 1]
      , [j 9,j 9,j 7,j 6,j 9,j 1,j 1,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just