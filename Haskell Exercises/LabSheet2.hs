import Data.Char

countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x:xs) = if x > 0
                        then 1 + countPositives xs
                            else countPositives xs

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x <= y
                  then x:y:ys
                    else y : insert x ys

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve [] = ([], [])
halve xs = (take lhx xs, drop lhx xs)
           where lhx = length xs `div` 2

msort :: (Ord a) => [a] -> [a]
msort [x] = [x]
msort xs = merge (msort left) (msort right)
           where (left, right) = halve xs

rotor :: Int -> String -> String
rotor n [] = error "Empty list"
rotor n xs | n > length xs = "greater than the length of the list"
           | n < 0 = error "Negative number can't be operated on"
     | n == 0 = xs 
           | otherwise = drop n xs ++ take n xs

makeKey :: Int -> [(Char, Char)]
makeKey n = zip alphabets (rotor n alphabets)
    where alphabets = ['A' .. 'Z']

lookUp :: Char -> [(Char, Char)] -> Char 
lookUp ch [] = ch
lookUp ch ((a, key) : aks) = if (ch == a) then key
            else lookUp ch aks

encipher :: Int -> Char -> Char
encipher n ch = lookUp ch (makeKey n)

normalise :: String -> String
normalise [] = []
normalise (ch : str) | (ch `elem` ['A' .. 'Z']) || (ch `elem` ['0' .. '9']) = ch : (normalise str)
                     | (ch `elem` ['a' .. 'z']) = toUpper ch : normalise str
                     | otherwise = normalise str
    
encipherStr :: Int -> String -> String
encipherStr n [] = []
encipherStr 0 str = str
encipherStr n str = [encipher n ch | ch <- newstr] where newstr = normalise str

decipherStr :: String -> [ String ]
decipherStr str = decipherStrHelper 0 str

decipherStrHelper :: Int -> String -> [ String ]
decipherStrHelper 26 str = []
decipherStrHelper n str = (encipherStr n str) : decipherStrHelper (n + 1) str