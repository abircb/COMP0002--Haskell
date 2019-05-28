mult :: (Num a) => [a] -> a
mult (x:xs) = foldr (\acc y -> acc * y) x xs

posList :: (Ord a, Num a) => [a] -> [a]
posList xs = filter (>0) xs

trueList :: [Bool] -> Bool
trueList xs = length (filter (==True) xs) == length xs

evenList :: [Int] -> Bool
evenList xs = length (filter even xs) == length xs

countPositives :: [Int] -> Int
countPositives xs = length (filter (>0) xs)

myLength :: [a] -> Int
myLength xs = foldr (+) 0 $ map (\x -> 1) xs

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:).f) []

myLength' :: [a] -> Int
myLength' xs = foldr ((+).(\x -> 1)) 0 xs



