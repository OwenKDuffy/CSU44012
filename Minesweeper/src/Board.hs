module Board where
    data Move = Move Char (Int, Int)
    data GameBoard = Board (Int, Int) [Int] [Char] -- dim, mines, CurrentOutput

    -- returnsLinear coords of neighbours to given tuple coord
    findNeighbours:: (Int, Int) -> (Int, Int) -> [Int]
    findNeighbours (i, j) dim = map (\y -> tupleToLinear dim y) (filter (\x -> validCell dim x) neighbours)
        where
            neighbours = [((i - 1), (j - 1)), ((i - 1), (j)), ((i - 1), (j + 1)),
                            ((i), (j - 1)), ((i), (j + 1)),
                            ((i + 1), (j - 1)), ((i + 1), (j)), ((i + 1), (j + 1))]


    findUnrevealedNeighbours :: Int -> [Char] -> (Int, Int) -> [Int]
    findUnrevealedNeighbours coord grid dim = filter (\n -> (grid !! n == '-')) (findNeighbours (linearToTuple coord dim) dim)


    -- convert tuple coords to index in linear space
    tupleToLinear:: (Int, Int) -> (Int, Int) -> Int
    tupleToLinear (width, _) (x, y) = (width * x) + y

    linearToTuple:: Int -> (Int, Int) -> (Int, Int)
    linearToTuple i (width, _) = ((i `div` width) , (i `mod` width))


    validCell :: (Int, Int) -> (Int, Int) -> Bool
    validCell (width, height) (x, y)    | (x >= 0 && x < width && y >= 0 && y < height) = True
                                        | otherwise = False
