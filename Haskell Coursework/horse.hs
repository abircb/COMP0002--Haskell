-- 1. Introduction

--Haskell Coursework 2018
--Written by Abir, on the 27th of November

-- 2. Basics

type Horse = [String]           -- Creating the Horse newtype

horse :: Horse                      -- instantiating a Horse type called horse
horse =   ["    ,//)    "
          ,"    ;;' \\   "
      ," ,;;' ( '\\  "
      ,"     / '\\-) "]

-- 3. Rotations

-- To print: pretty $ rotate horse
rotate :: Horse -> Horse
rotate ([]:_) = []
rotate h = map reverse $ transpose h

transpose :: Horse -> Horse     -- transposes the horse matrix
transpose  ([]:_)    = []
transpose h = (map head h) : transpose (map tail h)

-- To print: pretty $ mirror horse
mirror :: Horse -> Horse
mirror h = map reverse $ h


-- 4. Integer Sequences

-- First sequence
-- Sum of natural numbers(1, 3(1+2), 6(1+2+3), 10(1+2+3+4)...)
-- Known as Triangular Numbers because they can be arranged in an equilateral triangle

triangularNumbers :: Int -> [Int]
triangularNumbers x = take x $ map(generateTriangular) [1..]

generateTriangular :: Int -> Int    -- generates the nth triangular number
generateTriangular n = (n*(n+1)) `div` 2

-- Second Sequence
-- Prime number sequence (2,3,5,7...)

primeNumbers :: Int -> [Int]
primeNumbers x = take x $ checkPrime (2 : [3, 5..])

checkPrime :: [Int] -> [Int]        -- Filters out prime numbers from the list supplied
checkPrime (k:ys) = k : checkPrime [y|y <- ys, y `mod` k > 0]


-- 5. IO

pretty :: Horse -> IO ()
pretty h = putStr $ seperate h

seperate :: Horse -> String             -- Seperates every list in the horse(list of lists) using the newline character
seperate [] = ""
seperate (h:hs) = h ++  "\n" ++ seperate hs

-- To print: 1. horseSeq n triangularNumbers, where n >= 0
--           2. horseSeq n primeNumbers, where n >= 0

horseSeq :: (Int -> [Int]) -> Int -> Horse -> IO ()
horseSeq f n h = if x == -1
                    then putStr("")
                         else do horseSeq f (n-1) h
                                 pretty(generateHorses (f(n) !! x) h)
                 where x = length(f(n)) - 1


generateHorses :: Int -> Horse -> Horse -- generates n horses
generateHorses n hs = [concat $ replicate n h | h <- hs]