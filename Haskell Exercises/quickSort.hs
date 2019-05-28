quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort[a | a <- xs, a <= x] ++ [x] ++ quickSort[b | b <- xs, b > x]
