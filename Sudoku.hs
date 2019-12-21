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
-- checkMove board row col number = ((checkOccupied board row col) == False) && () && 

checkOccupied :: Sudoku -> Int -> Int -> Bool
checkOccupied (Sudoku a) row column = if (fst (a !! row !! column) == Nothing) then False else True

checkRow :: Sudoku -> Int -> Int -> Bool
checkRow (Sudoku a) rowno number = and [if (Just number) == value then False else True | (value, position) <- row]
                                    where row = a !! rowno

checkColumn :: [(Maybe Int, Int)] -> Int -> Bool
checkColumn column number = and [if (Just number) == value then False else True | (value, position) <- column]

checkJigsawPiece :: Sudoku -> Int -> Int -> Int -> Bool
checkJigsawPiece (Sudoku a) row column number = and [and [if (Just number) == value then False else True | (value, position)<- row, position == piece] | row <- a]
    where piece = snd (a !! row !! column)

printMiddleLineHelper :: (Maybe Int, Int) -> (Maybe Int, Int) -> (Maybe Int, Int) -> (Maybe Int, Int) -> IO ()
printMiddleLineHelper (_,a) (_,b) (_,c) (_,d)  
    | (a == b && a == c && a == d) = putStr ("    ")
    | (a == c && c == d && a /= b) = putStr ("   '")
    | (a /= c && c == d && a /= b) = putStr ("---'")
    | (a == b && c == d && a /= c) = putStr ("----")
    | (a == b && b == d && a /= c) = putStr ("---.")
    | (a == c && b == d && a /= b) = putStr ("   |") 
    | (b == c && c == d && a /= b) = putStr ("---'")
    | (a == b && b == c && a /= d) = putStr ("   .")
    | (a == b && c /= d && a /= d) = putStr ("---.")
    | (a /= b && c /= d && b == d) = putStr ("---:")
    | (a /= b && c /= d && a == c) = putStr ("   :")


printTopAndBottomLine :: [(Maybe Int, Int)] -> String -> Int -> IO ()
printTopAndBottomLine (x:[])   c _ = putStr ("---" ++ c)
printTopAndBottomLine (x:y:xs) c a =  do 
                                        if a == -1 then
                                            do putStr c
                                               printTopAndBottomLine (x:y:xs) c 1
                                        else
                                            if (snd x) == (snd y) then
                                                do putStr "----"   
                                                   printTopAndBottomLine (y:xs) c 1
                                            else
                                                do putStr ("---" ++ c)
                                                   printTopAndBottomLine (y:xs) c 1

printLine :: [(Maybe Int, Int)] -> Int -> IO ()
printLine ((Just a, b):[]) _ = putStr (" " ++ [(intToDigit a)]  ++ " |")
printLine ((Nothing, b):[]) _ = putStr (" ." ++ " |")
printLine (x:y:xs) a = do 
                         if a == -1 then
                            do putStr "|"
                               printLine (x:y:xs) 1
                         else
                            if (snd x) == (snd y) then
                                if (fst x) == Nothing then
                                    do putStr (" ." ++ "  ")
                                       printLine (y:xs) 1
                                else 
                                    do putStr (" " ++ [(intToDigit $ fromJust $ fst x)] ++ "  ")
                                       printLine (y:xs) 1
                            else
                                if (fst x) == Nothing then
                                    do putStr (" ." ++ " |")
                                       printLine (y:xs) 1
                                else
                                    do putStr (" " ++ [(intToDigit $ fromJust $ fst x)] ++ " |")
                                       printLine (y:xs) 1

printMiddleLine :: [(Maybe Int, Int)] -> [(Maybe Int, Int)] -> Int -> IO ()
printMiddleLine (a:[]) (c:[]) _ = if (snd a) == (snd c) then putStr "   |" else putStr "---:"
printMiddleLine (a:b:xs) (c:d:ys) e =  do
                                         if e == -1 then
                                            if (snd a == snd c) then
                                                do putStr "|"
                                                   printMiddleLine (a:b:xs) (c:d:ys) 1
                                            else
                                                do putStr ":"
                                                   printMiddleLine (a:b:xs) (c:d:ys) 1
                                         else
                                             do printMiddleLineHelper a b c d   
                                                printMiddleLine (b:xs) (d:ys) e

changeLine :: IO ()
changeLine = putStr "\n"

printSudoku :: Sudoku -> Int -> IO ()
printSudoku (Sudoku (x:[])) _ = do
                                  printLine (x) (-1)
                                  changeLine
                                  printTopAndBottomLine (x) ("'") (-1)
                                  changeLine
printSudoku (Sudoku (x:y:xs)) e = do
                                    if e == -1 then
                                        do printTopAndBottomLine (x) (".") (-1)
                                           changeLine
                                           printLine (x) (-1)
                                           changeLine
                                           printMiddleLine (x) (y) (-1)
                                           changeLine
                                           printSudoku (Sudoku (y:xs)) (1)
                                    else
                                        do printLine (x) (-1)
                                           changeLine
                                           printMiddleLine (x) (y) (-1)
                                           changeLine
                                           printSudoku (Sudoku (y:xs)) (1)


                           


-- -- game :: IO ()
-- -- game = do
-- --         fileContents <- readFile "map.txt"
-- --         let dat = lines fileContents
        






