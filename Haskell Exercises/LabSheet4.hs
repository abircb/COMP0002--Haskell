import Test.QuickCheck

halfEvens :: [ Int ] -> [ Int ]
halfEvens [ ] = [ ]
halfEvens (h : t) = if (mod h 2 == 0) then div h 2 : halfEvens t
				      else h : halfEvens t

halfEvens' :: [ Int ] -> [ Int ]
halfEvens' xs = map (\x -> if (even x) then (div x 2) else x) xs

prop_halfEvens :: [ Int ] -> Bool
prop_halfEvens xs = halfEvens xs == halfEvens' xs


inRange :: Int -> Int -> [ Int ] -> [ Int ]
inRange min max xs = [x | x <- xs, x >= min, x <= max]

inRange' :: Int -> Int -> [ Int ] -> [ Int ]
inRange' min max xs = filter (\x -> ( x >= min ) && ( x <= max ) ) xs

prop_inRange' :: Int -> Int -> [ Int ] -> Bool
prop_inRange' min max xs = inRange min max xs == inRange' min max xs

prop_inRange :: Int -> Int -> [ Int ] -> Bool
prop_inRange min max xs = res >= take len (cycle [min]) && res <= take len (cycle [max])
			  where res = inRange min max xs
	   			len = length res

countPositives :: [ Int ] -> Int
countPositives [ ] = 0
countPositives (h : t) = if (h > 0) then 1 + countPositives t
				    else countPositives t

countPositives' :: [ Int ] -> Int
countPositives' xs = length ( filter (\x -> x > 0)  xs )

prop_countPositives :: [ Int ] -> Bool
prop_countPositives xs = countPositives xs == countPositives' xs

rotor :: Int -> [Char] -> [Char]
rotor n [] = []
rotor 0 str = str
rotor n (c:str) | (0 <= n) && (n < length (c:str)) = rotor (n-1) (str ++ [c])

makeKey :: Int -> [ (Char, Char) ]
makeKey n = zip upperAl (rotor n upperAl)
		where upperAl = [ 'A' .. 'Z' ]

lookUp :: Char -> [ (Char, Char) ] -> Char
lookUp c [ ] = c
lookUp c ( (c1, c2) : pairs) 	| c == c1 = c2
 				| otherwise = lookUp c pairs

encipher :: Int -> Char -> Char
encipher n c = lookUp c ( makeKey n )
