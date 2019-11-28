module Funcs where

    import Data.Array
    import Data.List
    import Data.List.Split
    import System.Random
    import Data.Char


    data Move = Move Char (Int, Int)

    data GameBoard = Board (Int, Int) [Int] [Char] -- dim, mines, CurrentOutput

    play :: GameBoard -> IO()
    play board = do
        prettyPrintBoard board
        putStrLn "Next Move"
        command <- getLine
        let move = commandToMove command
        if endsGame move board
            then putStrLn "Game Over"
            else play (makeMove move board)


    endsGame :: Move -> GameBoard -> Bool
    endsGame (Move 'r' loc) (Board dim mines _) = isMine loc dim mines


    isMine :: (Int, Int) ->(Int, Int) -> [Int] -> Bool
    isMine (x, y) dims mineField | mineField !! (tupleToLinear dims (x, y)) == -1 = True
                            | otherwise = False

    makeMove :: Move -> GameBoard -> GameBoard
    makeMove mv (Board dim mines grid) = (Board dim mines (updateBoard mv (Board dim mines grid) (validMove mv dim)))


    updateBoard :: Move -> GameBoard -> Bool -> [Char]
    updateBoard _ (Board _ _ grid) False = grid -- if move invalid return as is
    updateBoard (Move 'r' (x, y)) (Board dim mines grid) True = replaceAt coord (intToDigit (mines !! coord)) grid
        where
            coord = (tupleToLinear dim (x, y))



    findNeighbours:: (Int, Int) -> (Int, Int) -> [Int]
    findNeighbours (i, j) dim = map (\y -> tupleToLinear dim y) (filter (\x -> validCell dim x) neighbours)
        where
            neighbours = [((i - 1), (j - 1)), ((i - 1), (j)), ((i - 1), (j + 1)),
                            ((i), (j - 1)), ((i), (j + 1)),
                            ((i + 1), (j - 1)), ((i + 1), (j)), ((i + 1), (j + 1))]

    tupleToLinear:: (Int, Int) -> (Int, Int) -> Int
    tupleToLinear (width, _) (x, y) = (width * x) + y


    validCell :: (Int, Int) -> (Int, Int) -> Bool
    validCell (width, height) (x, y)    | (x > 0 && x < width && y > 0 && y < height) = True
                                        | otherwise = False


    -- filterInvalidCells :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    -- filterInvalidCells (i, j) (x, y):xs | (x > 0 && x < i && j > 0 j < y) = (x, y):(filterInvalidCells (i, j) xs)
    --                                     | otherwise = filterInvalidCells (i, j) xs
    generateMines:: (Int, Int) -> Int -> [Int]
    -- populateMines dimensions numMines = sort (take numMines . nub $ (randomRs (0, numCells) g :: [Int]))
    --         where
    --             g = newStdGen
    --             (x, y) = dimensions
    --             numCells = x * y
    generateMines _ _ = [4, 5]

    populateMines:: [Int] -> [Int] -> (Int, Int) -> [Int] --MinesToPlace, MinesGrid, (dims)
    populateMines [] minesGrid (x, y) = minesGrid
    populateMines (x:[]) minesGrid dim = placeMine x minesGrid dim

    incrementMineNeighbours :: Int -> Int
    incrementMineNeighbours (-1) = (-1)
    incrementMineNeighbours x = x + 1

    placeMine:: Int -> [Int] -> (Int, Int) -> [Int]     -- mineLocation, prevMineField, dimensions
    placeMine i minesGrid (x,y) = replaceAt i (-1) updatedNeighbours
        where
            updatedNeighbours = map incrementMineNeighbours (findNeighbours ((i `div` x) , (i `mod` x)) (x,y))

    replaceAt :: Int -> a -> [a] -> [a] --index, toBeInserted, list
    replaceAt i  j listI = pre ++ [j] ++ newPost
        where
            (pre, post) = splitAt (i - 1) listI
            newPost = tail post

    validMove :: Move -> (Int, Int)-> Bool
    validMove (Move 'r' (i, j)) (x, y)  | i > 0 && i < x && j > 0 && j < y = True
    validMove (Move 'f' (i, j)) (x, y)  | i > 0 && i < x && j > 0 && j < y = True
                                        |otherwise = False

    commandToMove :: String -> Move
    commandToMove c = Move action (x, y)
        where
            action = head c
            location = splitOn ['_'] (tail c)
            x = digitToInt ((location !! 0) !! 0)
            y = digitToInt ((location !! 1) !! 0)



    initGrid:: (Int, Int) -> [Char]
    initGrid dim = (replicate i '-')
        where
            i = x * y
            (x, y) = dim
    -- linearTo2d:: [Int] -> Int -> [(Int, Int)]
    -- linearTo2d:: linearList -> width -> tupleList
    -- linearTo2d = map (getTuple width) linearList

    -- getTuple:: Int -> Int -> (Int, Int)
    -- getTuple:: dim -> x -> (y,z)
    -- getTuple = ((x `div` dim),(x % dim))

    prettyPrintBoard :: GameBoard -> IO()
    -- prettyPrintBoard ::
    prettyPrintBoard (Board dimensions mines grid) = mapM_ putStrLn (splitEvery x grid)
            where
                numCells = x * y
                (x,y) = dimensions

    -- grid :: Int -> Int -> [Int] -> [Int] -> [Char]
    -- grid x y m f = (cell x m f) ++ (grid (x+1) y m f)


    -- cell :: Int -> [Int] -> [Int] -> Char
    -- cell x m f  | revealed == False = '_'
    --             | revealed ==

    -- minesToString 0 mines numCells
    minesToString :: Int -> [Int] -> Int -> [Char] -- lastMine, list of Mines to place, end
    minesToString w ([]) y = (intsToString (y - w))
    minesToString w (x:xs) y = (intsToString (x - w))  ++ ['*'] ++ (minesToString (x+1) xs y)


    intsToString:: Int -> [Char]
    intsToString x = (replicate x '-')
