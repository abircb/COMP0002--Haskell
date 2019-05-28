import Data.Char

square :: Int -> Int
square x = x*x

pyth :: Int -> Int -> Int
pyth x y  = square x + square y

isTriple :: Int -> Int -> Int -> Bool
isTriple x y z = pyth x y == square z

isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny x y z = isTriple x y z || isTriple x z y || isTriple z y x

halfEvens :: [Int] -> [Int]
halfEvens xs = [if even x then div x 2 else x| x <- xs ]

inRange :: Integer -> Integer -> [Integer] -> [Integer]
inRange y z [] = []
inRange y z xs = [x | x <- xs, x >= y && x <= z]

countPositives :: [Integer] -> Int
countPositives [] = 0
countPositives xs = sum[1 | x <- xs, x > 0]

capitalised :: String -> String
capitalised xs = [toUpper y | y <- xs, y == head xs] ++  [toLower x | x <- xs, x /= head xs]

lowercase :: String -> String
lowercase word = [toLower w | w <- word]

title :: [String] -> [String]
title xs = [if length x > 3 || x == head xs
               then capitalised x
                else lowercase x | x <- xs]