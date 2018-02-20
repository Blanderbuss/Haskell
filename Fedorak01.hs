{-# OPTIONS_GHC -Wall #-}
module Fedorak01 where

-- Задача 1 -----------------------------------------
lastDigit :: Integer -> Integer
lastDigit x = mod x 10

-- Задача 2 -----------------------------------------
dropLastDigit :: Integer -> Integer
dropLastDigit x = div x 10

-- Задача 2 -----------------------------------------
toRevDigits :: Integer -> [Integer]
toRevDigits x = if x == 0 then [] else lastDigit(x):toRevDigits(dropLastDigit x)

-- Задача 3 -----------------------------------------
notDouble :: [Integer] -> [Integer]
notDouble xs = if null xs then []  else (head xs):(double (tail xs))

double :: [Integer] -> [Integer]
double xs = if null xs then [] else (2*(head xs)):(notDouble (tail xs))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = notDouble(xs)

-- Задача 4 -----------------------------------------
miniSum :: Integer -> Integer
miniSum x = if x == 0 then 0 else lastDigit x + miniSum(dropLastDigit x)

sumDigits :: [Integer] -> Integer
sumDigits xs = if null xs then 0 else sumDigits(tail xs) + miniSum(head xs)

-- Задача 5 -----------------------------------------
luhn :: Integer -> Bool
luhn x = mod (sumDigits(doubleEveryOther(toRevDigits(x)))) 10 == 0

-- Задача 6 -----------------------------------------
type Move = (Int,Int)

hanoi :: Integer -> Int -> Int -> Int -> [Move]
hanoi n beg int fin = if n == 0 then [] else (hanoi (n-1) beg fin int )++((beg, fin):(hanoi (n-1) int beg fin))

