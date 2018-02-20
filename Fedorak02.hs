{-# OPTIONS_GHC -Wall #-}
module Fedorak02 where

-- Код - просто список символів - десяткових цифр '0' ..'9'
type Code = String

-- Крок гри (Move) будує конструктор Move використовуючи спробу (Code) і два цілих:  
--    кількість "биків" і "корів"  у пропозиції-спробі по відношенню до коду-числа 
data Move = Move Code Int Int
          deriving (Show, Eq)

-- Задача 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = if (null c1) || (null c2) then 0 else (if head c1 == head c2 then 1 + exactMatches (tail c1) (tail c2) else exactMatches (tail c1) (tail c2))

-- Задача 2 -----------------------------------------
miniCount :: Code -> Char -> Int
miniCount c n = length(filter (==n) c)

countDigits :: Code -> [Int]
countDigits c = map (miniCount c) "0123456789"

-- Задача 3 ----------------------------------------- 
temp :: [Int] -> [Int] -> Int
temp l1 l2 = if (null l1) || (null l2) then 0 else (min (head l1) (head l2)) + temp (tail l1) (tail l2)

matches :: Code -> Code -> Int
matches c1 c2 = temp (countDigits c1) (countDigits c2)
 
-- Задача 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove c1 c2 = Move c2 (exactMatches c1 c2) (matches c1 c2 -(exactMatches c1 c2))

-- Задача 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move cd bl cw) c = (Move cd bl cw) == getMove c cd

-- Задача 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cdx = filter (isConsistent mv) cdx

-- Задача 7 -----------------------------------------
--extend :: [Code] -> Char -> [Code]
--extend cdx d = if null cdx then [[d]] else map (d:) cdx

--allCodes :: Int -> [Code]
--allCodes n = if n == 0 then [] else concat (map (extend (allCodes (n-1))) "0123456789")

extand :: [Code] -> Char -> [Code]
extand cdx d = if null cdx then [[d]] else map (d:) cdx

allCodes :: Int -> [Code]
allCodes n = if n == 0 then [] else (extand (allCodes (n-1)) '1')++(extand (allCodes (n-1)) '2')++(extand (allCodes (n-1)) '3')++(extand (allCodes (n-1)) '4') ++(extand (allCodes (n-1)) '5') ++(extand (allCodes (n-1)) '6') ++(extand (allCodes (n-1)) '7') ++(extand (allCodes (n-1)) '8') ++(extand (allCodes (n-1)) '9') ++(extand (allCodes (n-1)) '0')

   
-- Задача 7 -----------------------------------------
temp1 :: Code -> [Code] -> [Move]
temp1 c cdx = if (length cdx) == 1 then [getMove c (head cdx)] else (let cur = getMove c (head cdx) in cur:temp1 c (filterCodes cur cdx) )

solve :: Code -> [Move]
solve c = temp1 c (allCodes 4)
 
