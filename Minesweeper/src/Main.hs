module Main where

    import Funcs


    main:: IO()
    main = do
        let dimensions = (3, 3)
        let mines = populateMines dimensions 2
        let marked = []
        let gameBoard = Board dimensions mines marked
        play gameBoard
