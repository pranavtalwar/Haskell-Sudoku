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


constructPositionSudoku :: [String] -> [String] -> Sudoku
constructPositionSudoku positions values = Sudoku [[(if (values !! rowNo !! colNo == '.') then Nothing else Just (digitToInt (values !! rowNo !! colNo)), (digitToInt char)) | (char, colNo) <- zip row [0..length row - 1]] | (row, rowNo) <- zip positions [0..length positions - 1]]







