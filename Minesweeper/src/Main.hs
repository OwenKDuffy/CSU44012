module Main where

    import Funcs


    main:: IO()
    main = do
        let width = 3
        let height = 3
        let dimensions = (width, height)
        -- populateMines:: [Int] -> [Int] -> (Int, Int) -> [Int] --MinesToPlace, MinesGrid, (dims)
        let mines = populateMines (generateMines dimensions 2) (replicate (width * height) 0) dimensions
        let grid = initGrid dimensions
        -- data GameBoard = Board (Int, Int) [Int] [Char] -- dim, mines, CurrentOutput
        let gameBoard = Board dimensions mines grid
        play gameBoard
