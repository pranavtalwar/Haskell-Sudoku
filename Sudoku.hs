import Data.Maybe
import Data.Char

data Sudoku = Sudoku [[(Maybe Int, Int)]] deriving (Show)

lengthSudoku :: Sudoku -> Int
lengthSudoku (Sudoku a) = length a

getSudokuRows :: Sudoku -> [[(Maybe Int, Int)]]
getSudokuRows (Sudoku a) = a

getSudokuPositions :: [String] -> [String]
getSudokuPositions fileData = take 9 fileData

getSudokuValues :: [String] -> [String]
getSudokuValues fileData = reverse $ take 9 $ reverse fileData

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku empty
                where empty = [[(Nothing, -1) | num2 <- [1..9]] | num <- [1..9]]

constructSudoku :: [String] -> [String] -> Sudoku
constructSudoku positions values = Sudoku [[(if (values !! rowNo !! colNo == '.') then Nothing else Just (digitToInt (values !! rowNo !! colNo)), (digitToInt char)) | (char, colNo) <- zip row [0..length row - 1]] | (row, rowNo) <- zip positions [0..length positions - 1]]


checkSudokuOver :: Sudoku -> Bool
checkSudokuOver (Sudoku a) = and [and [(if value == Nothing then False else True) | (value, position) <- row ] | row <- a]

-- checkMove :: Sudoku -> Int -> Int -> Int -> Bool
-- checkMove (Sudoku a) row col number = 

checkOccupied :: Sudoku :: Int -> Int -> Bool
checkOccupied (Sudoku a) row column = if (a !! row !! column == Nothing) then True else False

checkRow :: [(Maybe Int, Int)] -> Int -> Bool
checkRow row number = and [if (Just number) == value then False else True | (value, position) <- row]

checkColumn :: [(Maybe Int, Int)] -> Int -> Bool
checkColumn column number = and [if (Just number) == value then False else True | (value, position) <- column]

checkJigsawPiece :: Sudoku -> Int -> Int -> Int -> Bool
checkJigsawPiece (Sudoku a) row column number = and [and [if (Just number) == value then False else True | (value, position)<- row, position == piece] | row <- a]
    where piece = snd (a !! row !! column)






