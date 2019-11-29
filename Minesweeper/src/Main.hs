module Main where

    import Funcs
    import CreateBoard


    main:: IO()
    main = do
        play (createBoard 10 10 10)
