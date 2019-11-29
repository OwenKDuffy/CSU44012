module Main where

    import Play
    import CreateBoard


    main:: IO()
    main = do
        putStrLn "Commands in the form of:\n[action] / [row no.] / [col no.]\nwhere actions are:\n'r' for reveal\n'f' for flag \n'a' to autoplay a move n.b you will need to provide dummy coordinates with this command but they will be ignored \nand\n0 <= [row no.] <= 9 \n0 <= [col no.] <= 9\nE.g. r/1/1"
        play (createBoard 10 10 10) -- this is the basic difficulty level in Minesweeper
