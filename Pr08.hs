module Pr08 where

isSubString::String -> String -> Bool
isSubString x y = any (isPrefix x) (tails y)

tails::String -> [String]
tails s | null s = [""]
        | otherwise = s:(tails (tail s))

isPrefix1::String -> String -> Bool
isPrefix1 x y = elem (reverse x) (tails (reverse y))

isPrefix::String -> String -> Bool
isPrefix "" _ = True
isPrefix _ "" = False
isPrefix (x:xs) (y:ys) | y == x = isPrefix xs ys
                       | otherwise = False

index::String -> String -> [Int]
index x y = map snd (filter fst (zip (map (isPrefix x) (tails y)) [0..]))