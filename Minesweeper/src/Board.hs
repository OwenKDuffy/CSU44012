module Board where
    data GameBoard = Board (Int, Int) [Int] [Char] -- dim, mines, CurrentOutput
