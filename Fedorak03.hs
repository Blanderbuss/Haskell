{-# OPTIONS_GHC -Wall #-}
module Fedorak03 where

data BinomTree a = Node a Int [BinomTree a]
               deriving (Eq, Ord, Show)
type BinomHeap a = [BinomTree a]

-- Задача 1 -----------------------------------------
value :: BinomTree a -> a
value (Node a _ _) = a

order :: BinomTree a -> Int
order (Node _ b _) = b

subTrees :: BinomTree a -> [BinomTree a]
subTrees (Node _ _ c) = c

combineTrees :: Ord a => BinomTree a -> BinomTree a -> BinomTree a
combineTrees bt1 bt2 = if bt1 <= bt2 then (Node (value bt1) ((order bt1) + 1) (bt2:(subTrees bt1))) 
else (Node (value bt2) ((order bt2) + 1) (bt1:(subTrees bt2)))
  
-- Задача 2 -----------------------------------------
extractMin :: Ord a => BinomHeap a -> a
extractMin bh = value(foldl min (head bh) bh)

-- Задача 3 ----------------------------------------- 

mergeHeaps :: Ord a => BinomHeap a -> BinomHeap a -> BinomHeap a
mergeHeaps bh1 bh2 | null bh1 = bh2
                    | null bh2 = bh1
                    | (order(head bh1) < order(head bh2)) = (head bh1):(mergeHeaps (tail bh1) bh2)
                    | (order(head bh2) < order(head bh1)) = (head bh2):(mergeHeaps bh1 (tail bh2))
                    | otherwise = mergeHeaps [combineTrees (head bh1) (head bh2)] (mergeHeaps (tail bh2) (tail bh1))
 

 --(mergeHeaps ((combineTrees (head bh1) (head bh2)):(tail bh1)) (tail bh2))
-- Задача 4 -----------------------------------------
insert :: Ord a => a -> BinomHeap a -> BinomHeap a
insert a bh = mergeHeaps bh ([Node a 0 []])

-- Задача 5 -----------------------------------------
deleteMin :: Ord a => BinomHeap a -> BinomHeap a
deleteMin bh = let toDel = (foldl min (head bh) bh) in mergeHeaps (subTrees toDel) (filter (/=toDel) bh) 

-- Задача 6 -----------------------------------------
formHeap :: Ord a => [a] -> BinomHeap a -> BinomHeap a
formHeap l bh = if null l then bh else formHeap (tail l) (insert (head l) bh)

formList :: Ord a => [a] -> BinomHeap a -> [a]
formList l bh = if null bh then l else ( (extractMin bh):formList (l) (deleteMin bh) ) 

binomSort :: Ord a => [a] -> [a]
binomSort l = formList [] (formHeap l [])

-- Задача 7 -----------------------------------------
--temp :: [Int] -> [Int] -> [Bool]
--temp l1 l2 = map (elem l1) l2

temp :: [Int] -> [Int] ->[Int]
temp l1 l2 = if null l2 then [] else ( if elem (head l2) l1 then 1:temp l1 (tail l2) else 0:temp l1 (tail l2))

toBinary :: BinomHeap a -> [Int]
toBinary bh = let list = binomSort(map order bh) in reverse (temp list [0..(last list)])

-----------------------------------------------------  
-- Приклади деяких дерев...
  
t1, t2, t3, t4, t5, t6, t7, t8 :: BinomTree Int
--  Зауваження: t7 - результат злиття t5 і t6

-- t1 .. t4 з'являються на Мал. 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 і t6 зліва на Мал.2; t7 - справа на Мал.2
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- Додаткове дерево...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Приклади деяких куп...

h1, h2, h3, h4, h5, h6, h7 :: BinomHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 показана на Мал.3...
h3 = [t1, t2, t4]

-- Дві додаткові купи використовуються далі. Вони зліва на Мал.4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 - результат злиття h4 і h5, справа на Мал.4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 показана на Мал.5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]  