module AutoSolver (chooseMove)
    where


    import Board
    import System.Random
    import Data.List
    import Data.Char
    import Funcs

    chooseMove :: [Char] -> (Int, Int) -> Move
    chooseMove grid dim = naive  grid dim 0


    -- a naive solving algorithm, should solve most basic grids i.e. 10x10 grid with 10 mines
    naive :: [Char] -> (Int, Int) -> Int -> Move
    naive  grid dim i  | i == ((length grid) - 1) = (Move 'r' (linearToTuple (chooseRandom grid) dim))
                    | grid !! i == '@' = naive  grid dim (i + 1)
                    | grid !! i == '-' = naive  grid dim (i + 1)
                    | digitToInt (grid !! i) >= 1 = cell i (digitToInt (grid !! i)) (findUnrevealedNeighbours i grid dim) (findSurroundingFlagged i grid dim) grid dim
                    | otherwise = naive  grid dim (i + 1)


    cell :: Int -> Int -> [Int] -> [Int] -> [Char] -> (Int, Int) -> Move
    cell cellpos cellVal unrevealedNeighbours flaggedNeighbours grid dim | length flaggedNeighbours == cellVal && length unrevealedNeighbours > 0 = (Move 'r' (linearToTuple (head unrevealedNeighbours) dim))
                                                                | length flaggedNeighbours == cellVal = naive  grid dim (cellpos + 1)
                                                                | (length unrevealedNeighbours + length flaggedNeighbours) == cellVal = (Move 'f' (linearToTuple (head unrevealedNeighbours) dim))
                                                                | otherwise = naive  grid dim (cellpos + 1)
    findSurroundingFlagged :: Int -> [Char] -> (Int, Int) -> [Int]
    findSurroundingFlagged coord grid dim = filter (\n -> (grid !! n == '@')) (findNeighbours (linearToTuple coord dim) dim)



    chooseRandom:: [Char] -> Int
    chooseRandom grid = head uncoveredCells
        where
            uncoveredCells = filter (\c -> grid !! c == '-') randomNums
            randomNums = take (numCells - 1) . nub $ randomRs (0, numCells) g
            g = mkStdGen 42
            numCells = length grid
