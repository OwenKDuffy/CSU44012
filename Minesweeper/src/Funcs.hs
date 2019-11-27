module Funcs where

    import Data.Array
    import Data.List
    import Data.List.Split
    import System.Random



    data GameBoard = Board (Int, Int) [Int] [Int]

    play :: GameBoard -> IO()
    play board = do
        prettyPrintBoard board
        putStrLn "Next Move"
        move <- getLine


    populateMines:: (Int, Int) -> Int -> [Int]
    -- populateMines dimensions numMines = sort (take numMines . nub $ (randomRs (0, numCells) g :: [Int]))
    --         where
    --             g = newStdGen
    --             (x, y) = dimensions
    --             numCells = x * y
    populateMines _ _ = [4, 5]


    -- linearTo2d:: [Int] -> Int -> [(Int, Int)]
    -- linearTo2d:: linearList -> width -> tupleList
    -- linearTo2d = map (getTuple width) linearList

    -- getTuple:: Int -> Int -> (Int, Int)
    -- getTuple:: dim -> x -> (y,z)
    -- getTuple = ((x `div` dim),(x % dim))

    prettyPrintBoard :: GameBoard -> IO()
    -- prettyPrintBoard ::
    prettyPrintBoard (Board dimensions mines marked) = mapM_ putStrLn (splitEvery x (minesToString 0 mines numCells))
            where
                numCells = x * y
                (x,y) = dimensions

    minesToString :: Int -> [Int] -> Int -> [Char] -- lastMine, list of Mines to place, end
    minesToString w ([]) y = (intsToString (y - w))
    minesToString w (x:xs) y = (intsToString (x - w))  ++ ['*'] ++ (minesToString (x+1) xs y)


    intsToString:: Int -> [Char]
    intsToString x = (replicate x '-')
