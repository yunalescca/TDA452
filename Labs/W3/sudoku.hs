
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
cell = frequency [(1, number),(9, nothing)]
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
prop_Sudoku sudoku = isSudoku sudoku

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






























