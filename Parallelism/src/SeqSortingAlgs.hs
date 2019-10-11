import Control.Parallel


--taken from http://learnyouahaskell.com/recursion
        quicksort :: (Ord a) => [a] -> [a]
        quicksort [] = []
        quicksort (x:xs) =
            let smallerSorted = par quicksort [a | a <- xs, a <= x]
                biggerSorted = par quicksort [a | a <- xs, a > x]
            in  smallerSorted ++ [x] ++ biggerSorted


-- taken from https://riptutorial.com/haskell/example/7552/merge-sort
        merge :: Ord a => [a] -> [a] -> [a]
        merge xs [] = xs
        merge [] ys = ys
        merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                            | otherwise = y:merge (x:xs) ys


	msort :: Ord a => [a] -> [a]
	msort [] = []
	msort [a] = [a]
	msort xs = merge (msort (firstHalf xs)) (msort (secondHalf xs))

	firstHalf  xs = let { n = length xs } in take (div n 2) xs
	secondHalf xs = let { n = length xs } in drop (div n 2) xs	
	

	main = print $ quicksort [59, 144, 136, 37, 34, 210, 273, 138, 378, 174, 207, 80, 276, 193, 500, 406, 127, 220, 217, 135, 264, 384, 213, 381, 225, 6, 41, 403, 215, 142, 27, 29, 479, 370, 133, 149, 303, 396, 283, 309, 368, 366, 418, 315, 201, 93, 187, 162, 43, 478, 203, 169, 486, 107, 232, 476, 334, 124, 179, 404, 227, 261, 433, 255, 467, 130, 402, 475, 386, 111, 307, 304, 48, 374, 63, 148, 134, 358, 308, 443, 196, 83, 249, 82, 116, 47, 180, 297, 208, 175, 172, 202, 449, 260, 453, 102, 419, 359, 380, 466]
