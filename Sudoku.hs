import Data.Maybe
import Data.Char
import System.Directory

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

checkBlankSudoku :: Sudoku -> Bool
checkBlankSudoku (Sudoku a) = and [and [if (value == Nothing) then True else False | (value, position) <- row] | row <- a]

constructSudoku :: [String] -> [String] -> Sudoku
constructSudoku positions values = Sudoku [[(if (values !! rowNo !! colNo == '.') then Nothing else Just (digitToInt (values !! rowNo !! colNo)), (digitToInt char)) | (char, colNo) <- zip row [0..length row - 1]] | (row, rowNo) <- zip positions [0..length positions - 1]]

checkSudokuOver :: Sudoku -> Bool
checkSudokuOver (Sudoku a) = and [and [(if value == Nothing then False else True) | (value, position) <- row ] | row <- a]

checkMove :: Sudoku -> Int -> Int -> Int -> Bool
checkMove board row col number = ((checkOccupied board row col) == False) && (checkRow board row number) && (checkColumn board col number) && (checkJigsawPiece board row col number)

checkOccupied :: Sudoku -> Int -> Int -> Bool
checkOccupied (Sudoku a) row column = if (fst (a !! row !! column) == Nothing) then False else True

checkRow :: Sudoku -> Int -> Int -> Bool
checkRow (Sudoku a) rowno number = and [if (Just number) == value then False else True | (value, position) <- row]
                                    where row = a !! rowno

checkColumn :: Sudoku -> Int -> Int -> Bool
checkColumn (Sudoku a) colno number = and [if ((Just number) == (fst $ (!!) row colno)) then False else True | row <- a]

checkJigsawPiece :: Sudoku -> Int -> Int -> Int -> Bool
checkJigsawPiece (Sudoku a) row column number = and [and [if (Just number) == value then False else True | (value, position)<- row, position == piece] | row <- a]
    where piece = snd (a !! row !! column)
   
makeMove :: Sudoku -> Int -> Int -> Int -> Sudoku
makeMove (Sudoku a) rowno colno number = Sudoku b
                  where b = [if (rowno == irow) then ([if (colno == icol) then ((Just number), position) else (value, position) | ((value, position), icol) <- zip row [0..8]]) else row | (row, irow) <- zip a [0..8]]

undoMove :: Sudoku -> Int -> Int -> Sudoku
undoMove (Sudoku a) rowno colno = Sudoku b
                  where b = [if (rowno == irow) then ([if (colno == icol) then (Nothing, position) else (value, position) | ((value, position), icol) <- zip row [0..8]]) else row | (row, irow) <- zip a [0..8]]


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


getFile :: IO String
getFile = do putStr "Enter the file name: "
             fileName <- getLine
             fileChecker <- doesFileExist fileName
             if (fileChecker == True) then
                do fileContents <- readFile fileName
                   return fileContents
             else 
                do putStrLn "Invalid input! The file does not exist in this directory"
                   getFile

getRowCol :: String -> IO Int
getRowCol message = do putStrLn message
                       x <- getChar
                       changeLine
                       if isDigit x then
                           do
                              let digit = digitToInt x
                              if (digit >= 0) && (digit < 9) then
                                 do return digit
                              else 
                                 do putStrLn "Please enter a digit in the range of 0 to 8"
                                    getRowCol message
                       else
                           do putStrLn "Please enter a digit!"
                              getRowCol message

getDigit :: String -> IO Int
getDigit message = do putStrLn message
                      x <- getChar
                      changeLine
                      if isDigit x then
                        do 
                           let digit = digitToInt x
                           if (digit > 0) && (digit <= 9) then
                              do return (digitToInt x)
                           else 
                              do putStrLn "Please enter a digit in the range of 1 to 9"
                                 getDigit message
                      else 
                        do putStrLn "Invalid input"
                           getDigit message

constructFile :: Sudoku -> String
constructFile (Sudoku a) = reverse $ drop 1 $ reverse $ unlines (positions ++ values)
                where positions = [[intToDigit position | (value, position) <- row] | row <- a]
                      values = [[if (value == Nothing) then '.' else (intToDigit $ fromJust value)| (value, position) <- row] | row <- a]

game :: Sudoku -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> IO ()
game (Sudoku a) moves undoneMoves = do
                                       putStrLn "Select one of the following options:"
                                       putStrLn "1. Load a board from file"
                                       putStrLn "2. Save a board to file"
                                       putStrLn "3. Quit the game"
                                       putStrLn "4. Make a move"
                                       putStr "Enter your choice: "
                                       option <- getChar
                                       changeLine
                                       if (option == '1') then
                                          do fileContents <- getFile
                                             let dat = lines fileContents
                                             let board = constructSudoku (getSudokuPositions dat) (getSudokuValues dat)
                                             putStrLn "Read board successfully!"
                                             putStrLn "Initial board:"
                                             printSudoku board (-1)
                                             game board moves undoneMoves
                                       else if (option == '2') then
                                             if checkBlankSudoku (Sudoku a) then
                                                do putStrLn "The board is empty hence you cannot save it!"
                                                   putStrLn "You must load a board from file first."
                                                   game (Sudoku a) moves undoneMoves
                                             else
                                                do putStr "Enter file to save to: "
                                                   fileName <- getLine
                                                   writeFile fileName (constructFile (Sudoku a))
                                                   putStrLn "File Saved"
                                                   game (Sudoku a) moves undoneMoves
                                       else if (option == '3') then 
                                             do 
                                             return ()
                                       else if (option == '4') then
                                             if checkBlankSudoku (Sudoku a) then 
                                                do putStrLn "The board is empty hence you cannot save it!"
                                                   putStrLn "You must load a board from file first."
                                                   game (Sudoku a) moves undoneMoves
                                             else 
                                                do putStrLn "Next move:"
                                                   row <- getRowCol "Row:"
                                                   col <- getRowCol "Column:"
                                                   number <- getDigit "Number:"
                                                   if (checkMove (Sudoku a) row col number) then 
                                                      do 
                                                         let board = makeMove (Sudoku a) row col number 
                                                         putStrLn "New board:"
                                                         printSudoku board (-1)
                                                         if checkSudokuOver board then
                                                            putStrLn "Congratulations you won the game!"
                                                         else
                                                            do game board moves undoneMoves
                                                   else
                                                      do putStrLn "Sorry, there is a conflict existing in your board."
                                                         putStrLn "Your current board:"
                                                         printSudoku (Sudoku a) (-1)
                                                         game (Sudoku a) moves undoneMoves
                                       else
                                             do putStrLn "Please enter a valid option!"
                                                game (Sudoku a) moves undoneMoves

main :: IO ()
main = game allBlankSudoku [] []

        






