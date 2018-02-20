{-# OPTIONS_GHC -Wall #-}
module Fedorak08 where

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Ord, Show)

-- Задача 1 -----------------------------------------
isPrefix::String -> String -> Bool
isPrefix "" _ = True
isPrefix _ "" = False
isPrefix (x:xs) (y:ys) | y == x = isPrefix xs ys
                       | otherwise = False

-- Задача 2 -----------------------------------------
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition x [] = ([],x,[])
partition [] x = ([],[],x)
partition (x:xs) (y:ys) | x /= y = ([],(x:xs),(y:ys))
                        | otherwise = ((x:a),b,c)
                        where (a,b,c) = partition xs ys

-- Задача 3 -----------------------------------------
suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes (x:xs) = (x:xs):(suffixes xs)

-- Задача 4 -----------------------------------------
isSubstring :: String -> String -> Bool
isSubstring x y = any (isPrefix x) ((""):(suffixes y))

-- Задача 5 -----------------------------------------
findSubstrings :: String -> String -> [Int]
findSubstrings x y = map snd (filter fst (zip (map (isPrefix x) (suffixes y)) [0..]))

-- Задача 6 -----------------------------------------
getIndices :: SuffixTree -> [Int]
getIndices (Leaf i) = [i]
getIndices (Node l) = concat (map (getIndices.snd) l)

-- Задача 7 -----------------------------------------
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' _ (Leaf _) = []
findSubstrings' _ (Node []) = []
findSubstrings' s (Node (x:xs)) | null b = getIndices (snd x)
                                | null c = findSubstrings' b (snd x)
                                | otherwise =  findSubstrings' s (Node xs)
                                where (_,b,c) = partition s (fst x)

-- Задача 8 -----------------------------------------
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s,i) (Node []) = Node [(s,(Leaf i))]
insert (s,i) (Node (x:xs)) | null p = Node (x:y1)
                           | p == a = Node ((a,(insert (b,i) t)):xs)
                           | p /= a = Node ((p,(Node [(c,t),(b,(Leaf i))])):xs)
                           where (a,t) = x
                                 (p,b,c) = partition s a
                                 (Node y1) = insert (s,i) (Node xs)

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

-- Задача 9 -----------------------------------------
longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring st = longest [x | x<-allx, length (findSubstrings' x st) >= 2]
                              where allx = getAll st

getAll:: SuffixTree -> [String]
getAll (Leaf _) = [[]]
getAll (Node []) = [[]]
getAll (Node (x:xs)) = (map ((fst x)++) (getAll $ snd x)) ++ (getAll $ Node xs)

longest :: [String] -> String
longest [] = []
longest xss = snd $ maximum $ [(length xs, xs) | xs <- xss]

allSubstrings::String->[String]
allSubstrings [] = []
allSubstrings s = suffixes s ++ allSubstrings (init s)

------------------------------------------------------
-- Приклади рядків і суфіксних дерев..

s1 :: String
s1 = "banana"

s2 :: String
s2  = "mississippi"

t1 :: SuffixTree
t1 = Node [("banana", Leaf 0), 
          ("a", Node [
                      ("na", Node [("na", Leaf 1),
                                   ("", Leaf 3)]),("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]