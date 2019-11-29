module Funcs where

    import Data.List
    import Data.List.Split
    import Data.Char
    import Rendering
    import Board
    data Move = Move Char (Int, Int)

    -- data GameBoard = Board (Int, Int) [Int] [Char] -- dim, mines, CurrentOutput

    play :: GameBoard -> IO()
    play board = do
        prettyPrintBoard board
        if gameWon board
            then
                putStrLn "You Win!!"
            else do
                putStrLn "Next Move"
                command <- getLine
                let move = commandToMove command
                -- putStrLn (moveToString move)
                if endsGame move board
                    then
                        putStrLn "Game Over"
                        -- gameOver board
                    else play (makeMove move board)


    gameWon :: GameBoard -> Bool
    gameWon (Board _ mines grid) = (length (filter (== -1) mines)) == (length (filter (== '-') grid))
    -- gameOver:: GameBoard -> IO()
    -- gameOver (Board (x, _) mines grid) = do
    --     mineLocations = filter (>=0) mines
    --     mapM_ putStrLn (splitEvery x grid)
    -- -- putStrLn "Game Over"
    -- -- prettyPrintBoard board


    moveToString:: Move -> String
    moveToString (Move c (x, y)) = c : (intToDigit x) : [(intToDigit y)]

    endsGame :: Move -> GameBoard -> Bool
    endsGame (Move 'r' loc) (Board dim mines _) = isMine loc dim mines
    endsGame (Move _ loc) _ = False


    isMine :: (Int, Int) ->(Int, Int) -> [Int] -> Bool -- locationToCheck, dimensionsOfMinefield, mineField -> isMine
    isMine loc dims mineField   | mineField !! (tupleToLinear dims loc) == -1 = True
                                | otherwise = False

    makeMove :: Move -> GameBoard -> GameBoard
    makeMove mv (Board dim mines grid) = (Board dim mines (updateBoard mv (Board dim mines grid) (validMove mv dim)))


    updateBoard :: Move -> GameBoard -> Bool -> [Char]
    updateBoard _ (Board _ _ grid) False = grid -- if move invalid return as is
    updateBoard (Move 'r' loc) (Board dim mines grid) True = reveal coord (Board dim mines grid)
        where   
            coord = (tupleToLinear dim loc) 
    updateBoard (Move 'f' loc) (Board dim mines grid) True = replaceAt coord '@' grid
        where
            coord = (tupleToLinear dim loc)

    reveal :: Int -> GameBoard -> [Char]
    reveal coord (Board dim mines grid) | grid !! coord == '-' && mines !! coord /= 0 = replaceAt coord (intToDigit (mines !! coord)) grid
                                        | grid !! coord == '-' && mines !! coord == 0 = floodFill (Board dim mines (replaceAt coord (intToDigit (mines !! coord)) grid)) (findUnrevealedNeighbours coord grid dim)

    findUnrevealedNeighbours :: Int -> [Char] -> (Int, Int) -> [Int]
    findUnrevealedNeighbours coord grid dim = filter (\n -> (grid !! n == '-')) (findNeighbours (linearToTuple coord dim) dim)

    floodFill :: GameBoard -> [Int] -> [Char]
    floodFill (Board _ _ grid) []     = grid
    floodFill (Board dim mines grid) (x:[]) | mines !! x /= 0 = replaceAt x (intToDigit (mines !! x)) grid
                                            | mines !! x == 0 = floodFill (Board dim mines (replaceAt x (intToDigit (mines !! x)) grid)) (findUnrevealedNeighbours x grid dim)
    floodFill (Board dim mines grid) (x:xs) | mines !! x /= 0 = floodFill (Board dim mines (replaceAt x (intToDigit (mines !! x)) grid)) xs
                                            | mines !! x == 0 = floodFill (Board dim mines (replaceAt x (intToDigit (mines !! x)) grid)) (union xs (findUnrevealedNeighbours x grid dim))



    -- returnsLinear coords of neighbours to given tuple coord
    findNeighbours:: (Int, Int) -> (Int, Int) -> [Int]
    findNeighbours (i, j) dim = map (\y -> tupleToLinear dim y) (filter (\x -> validCell dim x) neighbours)
        where
            neighbours = [((i - 1), (j - 1)), ((i - 1), (j)), ((i - 1), (j + 1)),
                            ((i), (j - 1)), ((i), (j + 1)),
                            ((i + 1), (j - 1)), ((i + 1), (j)), ((i + 1), (j + 1))]


    -- convert tuple coords to index in linear space
    tupleToLinear:: (Int, Int) -> (Int, Int) -> Int
    tupleToLinear (width, _) (x, y) = (width * x) + y

    linearToTuple:: Int -> (Int, Int) -> (Int, Int)
    linearToTuple i (width, _) = ((i `div` width) , (i `mod` width))


    validCell :: (Int, Int) -> (Int, Int) -> Bool
    validCell (width, height) (x, y)    | (x >= 0 && x < width && y >= 0 && y < height) = True
                                        | otherwise = False


    -- filterInvalidCells :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    -- filterInvalidCells (i, j) (x, y):xs | (x > 0 && x < i && j > 0 j < y) = (x, y):(filterInvalidCells (i, j) xs)
    --                                     | otherwise = filterInvalidCells (i, j) xs

    incrementAt :: [Int] -> [Int] -> [Int] -- currentGrid, indicesToBeIncremented -> updatedGrid
    incrementAt grid [] = grid
    incrementAt grid (x:[]) = replaceAt x (incrementMineNeighbour(grid !! x)) grid
    incrementAt grid (x:xs) = incrementAt (replaceAt x (incrementMineNeighbour(grid !! x)) grid) xs

    incrementMineNeighbour :: Int -> Int -- currentNumber of mines as neighbours -> newNumberOfMinesAsNeighbour
    incrementMineNeighbour (-1) = (-1)     -- if this square is a mine no change
    incrementMineNeighbour x = x + 1       -- else increment

    replaceAt :: Int -> a -> [a] -> [a] --index, toBeInserted, list
    replaceAt i j listI = x ++ [j] ++ y
        where
            x = take i listI
            y = drop (i+1) listI

    validMove :: Move -> (Int, Int)-> Bool
    validMove (Move 'r' (i, j)) (x, y)  | i >= 0 && i < x && j >= 0 && j < y = True
    validMove (Move 'f' (i, j)) (x, y)  | i >= 0 && i < x && j >= 0 && j < y = True
                                        |otherwise = False

    commandToMove :: String -> Move
    commandToMove c = Move action (x, y)
        where
            action = head c
            -- tail c == "/x/y"
            coords = drop 2 c
            -- xy = splitOn "/" coords
            x = digitToInt (coords !! 0)
            y = digitToInt (coords !! 2)
            -- x = digitToInt ((xy !! 0) !! 0)
            -- y = digitToInt ((xy !! 1) !! 0)




    -- linearTo2d:: [Int] -> Int -> [(Int, Int)]
    -- linearTo2d:: linearList -> width -> tupleList
    -- linearTo2d = map (getTuple width) linearList

    -- getTuple:: Int -> Int -> (Int, Int)
    -- getTuple:: dim -> x -> (y,z)
    -- getTuple = ((x `div` dim),(x % dim))


    -- grid :: Int -> Int -> [Int] -> [Int] -> [Char]
    -- grid x y m f = (cell x m f) ++ (grid (x+1) y m f)


    -- cell :: Int -> [Int] -> [Int] -> Char
    -- cell x m f  | revealed == False = '_'
    --             | revealed ==
