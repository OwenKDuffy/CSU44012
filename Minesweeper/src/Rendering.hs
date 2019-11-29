module Rendering where

    import Data.List.Split
    import Board

    -- data GameBoard = Board (Int, Int) [Int] [Char] -- dim, mines, CurrentOutput

    prettyPrintBoard :: GameBoard -> IO()
    -- prettyPrintBoard ::
    prettyPrintBoard (Board (x, _) _ grid) = mapM_ putStrLn (splitEvery x (map printingReplacements grid))

    printingReplacements:: Char -> Char
    printingReplacements '0' = ' '
    printingReplacements x = x

    minesToString :: Int -> [Int] -> Int -> [Char] -- lastMine, list of Mines to place, end
    minesToString w ([]) y = (intsToString (y - w))
    minesToString w (x:xs) y = (intsToString (x - w))  ++ ['*'] ++ (minesToString (x+1) xs y)

    intsToString:: Int -> [Char]
    intsToString x = (replicate x '-')
