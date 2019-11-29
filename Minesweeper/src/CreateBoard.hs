module CreateBoard (createBoard) where

    import Funcs
    import Board
    import System.Random
    import Data.List

    createBoard :: Int -> Int -> Int -> GameBoard
    createBoard width height numMines = Board dimensions mineField grid
        where
            dimensions = (width, height)
            emptyMineField = (replicate (width * height) 0)
            mineField = populateMines (generateMines dimensions 10) emptyMineField dimensions
            grid = initGrid dimensions

    generateMines:: (Int, Int) -> Int -> [Int]
    generateMines (x, y) numMines = (take numMines . nub $ randomNums)
        where
            randomNums = randomRs (0, numCells) g
            g = mkStdGen 42
            numCells = x * y
    -- generateMines _ _ = [74, 76, 81, 10, 3, 39, 6, 92, 4, 71]

    populateMines:: [Int] -> [Int] -> (Int, Int) -> [Int] --MinesToPlace, MinesGrid, (dims)
    populateMines [] minesGrid (x, y) = minesGrid
    populateMines (x:[]) minesGrid dim = (placeMine x minesGrid dim)
    populateMines (x:xs) minesGrid dim = populateMines xs (placeMine x minesGrid dim) dim

    placeMine:: Int -> [Int] -> (Int, Int) -> [Int]     -- mineLocation, prevMineField, dimensions -> newMineField
    placeMine mine minesGrid dim = replaceAt mine (-1) (incrementAt minesGrid neighbours)
        where
            neighbours = findNeighbours (linearToTuple mine dim) dim
    -- placeMine i minesGrid (x,y) = replaceAt i (-1) updatedNeighbours
    --     where
    --         updatedNeighbours = map incrementMineNeighbours (findNeighbours ((i `div` x) , (i `mod` x)) (x,y))


    initGrid:: (Int, Int) -> [Char]
    initGrid (x, y) = (replicate (x * y) '-')
