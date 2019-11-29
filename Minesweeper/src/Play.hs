module Play where

    import Rendering
    import Board
    import AutoSolver
    import Data.List.Split
    import Data.Char
    import Funcs


    -- Commands in the form of:
    -- [action] "/" [row no.] "/" [col no.]
    -- where actions are:
    -- 'r' for reveal
    -- 'f' for flag
    -- 'a' to autoplay a move n.b you will need to provide dummy coordinates with this command but they will be ignored
-- 
    -- and
    -- 0 <= [row no.] <= 9
    -- 0 <= [col no.] <= 9
-- 
    -- E.g. r/1/1
-- 
    play :: GameBoard -> IO()
    play board = do
        prettyPrintBoard board
        if gameWon board
            then
                putStrLn "You Win!!"
            else do
                putStrLn "Next Move"
                command <- getLine
                let move = resolveMove (commandToMove command) board
                if endsGame move board
                    then
                        gameOver board
                    else play (makeMove move board)

    endsGame :: Move -> GameBoard -> Bool
    endsGame (Move 'r' loc) (Board dim mines _) = isMine loc dim mines
    endsGame (Move _ loc) _ = False

    minedGrid :: [Int] -> [Char] -> [Char]
    minedGrid mines grid  = map (\(a,b) -> fillMines a b) $ zip mines grid

    fillMines:: Int -> Char -> Char
    fillMines (-1) _ = '*'
    fillMines _ c = c

    isMine :: (Int, Int) ->(Int, Int) -> [Int] -> Bool -- locationToCheck, dimensionsOfMinefield, mineField -> isMine
    isMine loc dims mineField   | mineField !! (tupleToLinear dims loc) == -1 = True
                                | otherwise = False


    makeMove :: Move -> GameBoard -> GameBoard
    makeMove mv (Board dim mines grid) = (Board dim mines (updateBoard mv (Board dim mines grid) (validMove mv dim)))


    commandToMove :: String -> Move
    commandToMove c = Move action (x, y)
        where
            action = head c
            -- tail c == "/x/y"
            coords = drop 2 c
            -- xy = splitOn "/" coords
            x = digitToInt (coords !! 0)
            y = digitToInt (coords !! 2)

    resolveMove :: Move -> GameBoard -> Move
    resolveMove (Move 'a' _) (Board dim _ grid) = chooseMove grid dim
    resolveMove mv _ = mv

    gameWon :: GameBoard -> Bool
    gameWon (Board _ mines grid) = (length (filter (== -1) mines)) == (length (filter (== '-') grid))

    gameOver:: GameBoard -> IO()
    gameOver (Board (x, _) mines grid) = do
        mapM_ putStrLn ((splitEvery x (map printingReplacements (minedGrid mines grid))))

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
