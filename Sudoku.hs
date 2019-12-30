import Data.Maybe
import Data.Char
import Data.List
import System.Directory
import System.IO

data Sudoku = Sudoku [[(Maybe Int, Int)]] deriving (Show)

-- Helper function that extracts rows from a Sudoku datatype and returns a list of lists
getSudokuRows :: Sudoku -> [[(Maybe Int, Int)]]
getSudokuRows (Sudoku a) = a

-- The functions below check whether the file being used to load a board is valid or not
-- Checks whether the positions in given filedata are valid or not
checkPositions :: [String] -> Bool
checkPositions positions = and [and [isDigit position && digitToInt position >=0 && digitToInt position < 9 | position <- line] | line <- positions] && and [length digitgroup ==9 | digitgroup <- group $ sort $ concat positions]

-- Checks whether the positions in given filedata are valid or not
checkValues :: [String] -> Bool
checkValues values = and [and [value == '.' || (isDigit value && digitToInt value >=1 && digitToInt value <= 9) | value <- row] | row <- values]

-- Checks whether all the data in the given file is valid or not
checkFile :: [String] -> Bool
checkFile fileContents = ((length fileContents) == 18) && (and [length line == 9 | line <- fileContents]) && (checkPositions $ take 9 fileContents)  && (checkValues $ drop 9 fileContents)

-- The functions below help in constructing a JigSaw Sudoku board from filedata and vice versa
-- Helper function that gets the jigsaw positions
getSudokuPositions :: [String] -> [String]
getSudokuPositions fileData = take 9 fileData

-- Helper function that gets the state of the numbers on the Sudoku Board
getSudokuValues :: [String] -> [String]
getSudokuValues fileData = drop 9 fileData

-- Constructs a JigSaw Sudoku board given positions and values of each element
constructSudoku :: [String] -> [String] -> Sudoku
constructSudoku positions values = Sudoku [[(if (values !! rowNo !! colNo == '.') then Nothing else Just (digitToInt (values !! rowNo !! colNo)), (digitToInt char)) | (char, colNo) <- zip row [0..length row - 1]] | (row, rowNo) <- zip positions [0..length positions - 1]]

-- Constructs filedata in the proper format from a given JigSaw Board
constructFile :: Sudoku -> String
constructFile (Sudoku a) = reverse $ drop 1 $ reverse $ unlines (positions ++ values)
                where positions = [[intToDigit position | (value, position) <- row] | row <- a]
                      values = [[if (value == Nothing) then '.' else (intToDigit $ fromJust value)| (value, position) <- row] | row <- a]

-- Generates a blank JigSaw Sudoku
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku empty
   where empty = [[(Nothing, -1) | column <- [1..9]] | row <- [1..9]]

-- Checks whether a board is blank or not
checkBlankSudoku :: Sudoku -> Bool
checkBlankSudoku (Sudoku a) = and [and [isNothing value | (value, position) <- row] | row <- a]

-- Checks whether the whole JigSaw Sudoku board is filled up or not (Winning Condition)
checkSudokuOver :: Sudoku -> Bool
checkSudokuOver (Sudoku a) = and [and [isJust value | (value, position) <- row ] | row <- a]

-- The functions below check the validity of the move
-- Checks whether a position is Occupied or not
checkOccupied :: Sudoku -> Int -> Int -> Bool
checkOccupied (Sudoku a) row column = isJust $ fst $ a !! row !! column

-- Checks whether a number can be used in a row or not
checkRow :: Sudoku -> Int -> Int -> Bool
checkRow (Sudoku a) rowno number = and [not $ number == fromJust value | (value, position) <- row, isJust value]
                                    where row = a !! rowno

-- Checks whether a number can be used in a column or not
checkColumn :: Sudoku -> Int -> Int -> Bool
checkColumn (Sudoku a) colno number = and [not $ number == (fromJust $ fst $ row !! colno) | row <- a, isJust $ fst $ row !! colno]

-- Checks whether a number can be used in a JigSaw Piece or not
checkJigsawPiece :: Sudoku -> Int -> Int -> Int -> Bool
checkJigsawPiece (Sudoku a) row column number = and [and [not $ number == fromJust value | (value, position)<- row, position == piece && isJust value] | row <- a]
    where piece = snd (a !! row !! column)

-- Checks whether a move is valid or not
checkMove :: Sudoku -> Int -> Int -> Int -> Bool
checkMove board row col number = ((checkOccupied board row col) == False) && (checkRow board row number) && (checkColumn board col number) && (checkJigsawPiece board row col number)

-- Returns a new JigSaw Sudoku board after a move is made
makeMove :: Sudoku -> Int -> Int -> Int -> Sudoku
makeMove (Sudoku a) rowno colno number = Sudoku b
                  where b = [if (rowno == irow) then ([if (colno == icol) then ((Just number), position) else (value, position) | ((value, position), icol) <- zip row [0..8]]) else row | (row, irow) <- zip a [0..8]]

-- Returns a new JigSaw Sudoku board after a move is undone 
undoMove :: Sudoku -> Int -> Int -> Sudoku
undoMove (Sudoku a) rowno colno = Sudoku b
   where b = [if (rowno == irow) then ([if (colno == icol) then (Nothing, position) else (value, position) | ((value, position), icol) <- zip row [0..8]]) else row | (row, irow) <- zip a [0..8]]

-- The following functions helps in solving a given JigSaw Sudoku Board
-- Helper function finds the possible numbers that can be inserted in a given position
possibleNumbers :: Sudoku -> Int -> Int -> [Int]
possibleNumbers (Sudoku a) row column = if (isNothing $ fst $ a !! row !! column) then [1..9] \\ (nub $ concat [rownums, colnums, jigsawnums]) else [fromJust $ fst $ a!! row !! column]
   where rownums = [fromJust value | (value, position) <- a !! row, isJust value] 
         colnums =  [fromJust $ fst $ row !! column | row <- a, isJust $ fst $ row !! column]
         piecenumber = snd $ a !! row !! column
         jigsawnums = concat [[fromJust value | (value,position) <- row, position == piecenumber && isJust value]| row <- a]
  
-- Finds the next element that must be filled up given the coordinates
nextElement :: Sudoku -> Int -> Int -> (Int, Int)               
nextElement (Sudoku a) 8 8 = (8,8)
nextElement (Sudoku a) row 8 = if isNothing $ fst $ a !! (row +1)!! 0 then (row + 1, 0) else nextElement (Sudoku a) (row+1) 0
nextElement (Sudoku a) row column 
   | isNothing $ fst $ a !! row !! (column + 1) = (row, column + 1)
   | otherwise = nextElement (Sudoku a) (row) (column+1)
                  
-- Helper functions to solve a JigSaw Sudoku Board                              
solve :: Sudoku -> Int -> Int -> [Int] -> Sudoku
solve board 8 8 [] = allBlankSudoku
solve board 8 8 (x:[]) = makeMove board 8 8 x
solve board 8 8 (x:_) = allBlankSudoku
solve board _ _ [] = allBlankSudoku
solve board row column (x:xs)
   | (checkBlankSudoku newboard) = solve board row column xs
   | otherwise = newboard
   where newboard = solveNext (makeMove board row column x) row column

-- Helper function for solving, returns a new board
solveNext :: Sudoku -> Int -> Int -> Sudoku
solveNext sud nrow ncolumn = solve sud (fst(nextElement sud nrow ncolumn)) (snd(nextElement sud nrow ncolumn)) (possibleNumbers sud (fst(nextElement sud nrow ncolumn)) (snd(nextElement sud nrow ncolumn)))

-- Solves a JigSaw Sudoku board if possible         
solver:: Sudoku -> Sudoku
solver board = solve board 0 0 (possibleNumbers board 0 0)
                                         
-- Finds the first empty cell in a board according to the provided coordinates
firstEmpty :: Sudoku -> Int -> Int -> (Int, Int)
firstEmpty (Sudoku a) row 9 = firstEmpty (Sudoku a) (row + 1) 0
firstEmpty (Sudoku a) row column
   | isNothing $ fst $ a !! row !! column = (row, column)
   | otherwise = firstEmpty (Sudoku a) row (column + 1)

-- The following are functions that help in printing to the console
-- Helper functions for changing a line
changeLine :: IO ()
changeLine = putStr "\n"

-- Helper function to print error message when board is not loaded
emptyBoardError :: String -> IO ()
emptyBoardError message = do putStrLn message
                             putStrLn "You must load a board from file first."

-- Helper function to print the first and last line of a JigSaw Sudoku Board
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

-- Helper function to print a row in the JigSaw Sudoku Board
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

-- Helper function for a printing a line between two rows of a JigSaw board
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

-- Function to print the line between two rows of a JigSaw Board
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

-- Function that prints a JigSaw Sudoku board
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

-- The following functions take user input and check their validity                                          
-- Gets a file and returns its contents if the file exists and its contents are valid
getFile :: IO String
getFile = do putStr "Enter the file name: "
             hFlush stdout
             fileName <- getLine
             fileChecker <- doesFileExist fileName
             if (fileChecker) then
                do fileContents <- readFile fileName
                   let newFileContents = lines fileContents
                   if (checkFile newFileContents) then 
                     do return fileContents
                   else 
                     do putStrLn "Invalid input! The file contents are not valid"
                        getFile
             else 
                do putStrLn "Invalid input! The file does not exist in this directory"
                   getFile

-- Gets row and column input from the user and checks the input validity
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

-- Get a number input from the user and checks the input validity                              
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

-- Provides options to the user and then handles it depending on the user input
game :: Sudoku -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> IO ()
game (Sudoku a) moves undoneMoves = do
                                       hSetBuffering stdin NoBuffering
                                       putStrLn "Select one of the following options:"
                                       putStrLn "1. Load a board from file"
                                       putStrLn "2. Save a board to file"
                                       putStrLn "3. Quit the game"
                                       putStrLn "4. Make a move"
                                       putStrLn "5. Undo a move"
                                       putStrLn "6. Redo a move"
                                       putStrLn "7. Solve board"
                                       putStrLn "8. Hint"
                                       putStrLn "9. Display Board"
                                       putStr "Enter your choice: "
                                       hFlush stdout
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
                                                hFlush stdout
                                                fileName <- getLine  
                                                writeFile fileName (constructFile (Sudoku a))
                                                putStrLn "File Saved!"
                                                game (Sudoku a) moves undoneMoves
                                       else if (option == '3') then 
                                          do 
                                             return ()
                                       else if (option == '4') then
                                          if checkBlankSudoku (Sudoku a) then 
                                             do emptyBoardError "The board is empty hence you cannot make a move!"
                                                game (Sudoku a) moves undoneMoves
                                          else 
                                             do putStrLn "Next move:"
                                                row <- getRowCol "Row:"
                                                col <- getRowCol "Column:"
                                                number <- getDigit "Number:"
                                                if (checkMove (Sudoku a) row col number) then 
                                                   do 
                                                      let board = makeMove (Sudoku a) row col number
                                                      let newmoves =  (row,col,number):moves
                                                      putStrLn "New board:"
                                                      printSudoku board (-1)
                                                      if checkSudokuOver board then
                                                         putStrLn "Congratulations you won the game!"
                                                      else
                                                         do game board newmoves []
                                                else
                                                   do putStrLn "Sorry, there is a conflict existing in your board."
                                                      putStrLn "Your current board:"
                                                      printSudoku (Sudoku a) (-1)
                                                      game (Sudoku a) moves undoneMoves
                                       else if (option == '5') then
                                          if (checkBlankSudoku (Sudoku a)) then
                                             do emptyBoardError "The board is empty hence you cannot undo a move!"
                                                game (Sudoku a) moves undoneMoves
                                          else
                                             if moves == [] then
                                                do putStrLn "You have not made any moves to undo"
                                                   printSudoku (Sudoku a) (-1)
                                                   game (Sudoku a) moves undoneMoves
                                             else 
                                                do
                                                   let lastMove = head moves
                                                   let (row, col, number) = lastMove
                                                   let newMoves = drop 1 moves
                                                   let newUndoneMoves = lastMove : undoneMoves
                                                   let board = undoMove (Sudoku a) row col
                                                   putStrLn "Move has been undone!"
                                                   printSudoku board (-1)
                                                   game board newMoves newUndoneMoves
                                       else if (option == '6') then 
                                          if (checkBlankSudoku (Sudoku a)) then
                                             do emptyBoardError "The board is empty hence you cannot redo a move!"
                                                game (Sudoku a) moves undoneMoves
                                          else
                                             if undoneMoves == [] then
                                                do putStrLn "You have not undone any moves to redo"
                                                   printSudoku (Sudoku a) (-1)
                                                   game (Sudoku a) moves undoneMoves
                                             else 
                                                do
                                                   let lastUndoneMove = head undoneMoves
                                                   let (row, col, number) = lastUndoneMove
                                                   let newUndoneMoves = drop 1 undoneMoves
                                                   let newMoves = lastUndoneMove : moves
                                                   let board = makeMove (Sudoku a) row col number
                                                   putStrLn "Move has been redone!"
                                                   printSudoku board (-1)
                                                   game board newMoves newUndoneMoves
                                       else if (option == '7') then
                                          if (checkBlankSudoku (Sudoku a)) then
                                             do emptyBoardError "The board is empty hence it cannot be solved"
                                                game (Sudoku a) moves undoneMoves
                                          else
                                             do 
                                                let solvedboard = solver (Sudoku a)
                                                if ((checkBlankSudoku solvedboard) == False) then
                                                   do
                                                      printSudoku solvedboard (-1)
                                                      putStrLn "This is the solution!"
                                                else
                                                   do putStrLn "No solution for this board"
                                       else if (option == '8') then 
                                          if (checkBlankSudoku (Sudoku a)) then
                                             do emptyBoardError "The board is empty hence hint cannot be given"
                                                game (Sudoku a) moves undoneMoves
                                          else 
                                             do let solvedboard = solver (Sudoku a)
                                                if ((checkBlankSudoku solvedboard) == False) then
                                                   do 
                                                      let (hintr, hintc) = firstEmpty (Sudoku a) 0 0 
                                                      let hintNum = fromJust $ fst $ (getSudokuRows solvedboard) !! hintr !! hintc
                                                      putStrLn ("You can put " ++ show hintNum ++ " on row number: " ++ show hintr ++ " and column number: " ++ show hintc)
                                                      game (Sudoku a) moves undoneMoves
                                                else 
                                                   do putStrLn "No possible hint for this situation"
                                                      game (Sudoku a) moves undoneMoves
                                       else if (option == '9') then
                                          if (checkBlankSudoku (Sudoku a)) then
                                             do emptyBoardError "The board is empty hence cannot be displayed"
                                                game (Sudoku a) moves undoneMoves
                                          else
                                             do putStrLn "The current board is:"
                                                printSudoku (Sudoku a) (-1)
                                                game (Sudoku a) moves undoneMoves
                                       else
                                             do putStrLn "Please enter a valid option!"
                                                game (Sudoku a) moves undoneMoves

-- Main function , calls the game function with an empty board, empty moves and empty undone moves                                                
main :: IO ()
main = game allBlankSudoku [] []