module Funcs where

    import Data.List
    import Data.List.Split
    import Data.Char
    import Rendering
    import Board
    -- import AutoSolver


    -- data GameBoard = Board (Int, Int) [Int] [Char] -- dim, mines, CurrentOutput


    -- putStrLn "Game Over"
    -- prettyPrintBoard board


    moveToString:: Move -> String
    moveToString (Move c (x, y)) = c : (intToDigit x) : [(intToDigit y)]



    floodFill :: GameBoard -> [Int] -> [Char]
    floodFill (Board _ _ grid) []     = grid
    floodFill (Board dim mines grid) (x:[]) | mines !! x /= 0 = replaceAt x (intToDigit (mines !! x)) grid
                                            | mines !! x == 0 = floodFill (Board dim mines (replaceAt x (intToDigit (mines !! x)) grid)) (findUnrevealedNeighbours x grid dim)
    floodFill (Board dim mines grid) (x:xs) | mines !! x /= 0 = floodFill (Board dim mines (replaceAt x (intToDigit (mines !! x)) grid)) xs
                                            | mines !! x == 0 = floodFill (Board dim mines (replaceAt x (intToDigit (mines !! x)) grid)) (union xs (findUnrevealedNeighbours x grid dim))



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
