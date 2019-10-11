module SeqSortingAlgs
    ( quicksort,
        mergesort
    ) where


--taken from http://learnyouahaskell.com/recursion
        quicksort :: (Ord a) => [a] -> [a]
        quicksort [] = []
        quicksort (x:xs) =
            let smallerSorted = quicksort [a | a <- xs, a <= x]
                biggerSorted = quicksort [a | a <- xs, a > x]
            in  smallerSorted ++ [x] ++ biggerSorted


-- taken from https://riptutorial.com/haskell/example/7552/merge-sort
        mergesort :: Ord a => [a] -> [a] -> [a]
        mergesort xs [] = xs
        mergesort [] ys = ys
        mergesort (x:xs) (y:ys) | x <= y    = x:mergesort xs (y:ys)
                            | otherwise = y:mergesort (x:xs) ys
