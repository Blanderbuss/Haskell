{-# OPTIONS_GHC -Wall #-}
module Fedorak13 where

import Data.List

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
simplify :: RE -> RE   
simplify Null = Null
simplify (Term c) = Term c
simplify (Seq r1 r2) = Seq (simplify r1) (simplify r2)
simplify (Alt r1 r2) = Alt (simplify r1) (simplify r2)
simplify (Rep r) = Rep $ simplify r
simplify (Plus r) = Seq (simplify r) (Rep $ simplify r)
simplify (Opt r) = Alt (simplify r) Null

-- Задача 2 -----------------------------------------
startState     :: Automation -> State
terminalStates :: Automation -> [State]
transitions    :: Automation -> [Transition] 

startState (s, _, _) = s
terminalStates (_, xs, _) = xs
transitions (_, _, xt) = xt

-- Задача 3 -----------------------------------------
isTerminal :: State -> Automation -> Bool 
isTerminal s (_, xs, _) = elem s xs

-- Задача 4 -----------------------------------------
get1 :: (a,b,c) -> a
get1 (a,_,_) = a

get3 :: (a,b,c) -> c
get3 (_,_,c) = c

transitionsFrom :: State -> Automation -> [Transition]
transitionsFrom st (_, _, ts) = filter ((==st).get1) ts

-- Задача 5 -----------------------------------------
labels :: [Transition] -> [Label]
labels [] = []
labels (t:ts) | cur /= Eps && not (elem cur next) = cur:next
              | otherwise = next
              where next = labels ts
                    cur = get3 t

-- Задача 6 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts a s = acc a s

acc :: Automation -> String -> Bool
acc (str,ter,_) [] = elem str ter
acc (str,ter,ts) s = foldl (||) False (map (try (str,ter,ts) s) (transitionsFrom str (str,ter,ts)))

try :: Automation -> String -> Transition -> Bool
try (str,ter,_) [] _ = elem str ter
try (_,ter,ts) s (_,s2,Eps) = acc (s2,ter,ts) s
try (_,ter,ts) (s:ss) (_,s2,C ch) = if s == ch then acc (s2,ter,ts) ss else False

-- Задача 7 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null beg fin nxt = ([(beg,fin, Eps)], nxt)
make (Term ch) beg fin nxt = ([(beg,fin, C ch)], nxt)
make (Seq r1 r2) beg fin nxt = (((nxt,k1,Eps):ts1++ts2),k2)
                             where (ts1, k1) = make r1 beg nxt (nxt+1)
                                   (ts2, k2) = make r2 k1 fin (k1+1)
make (Alt r1 r2) beg fin nxt = (((beg,nxt,Eps):(beg,(nxt+2),Eps):((nxt+1),fin,Eps):((nxt+3,fin,Eps)):ts1++ts2),k2)
                             where (ts1, k1) = make r1 nxt (nxt+1) (nxt+4)
                                   (ts2, k2) = make r2 (nxt+2) (nxt+3) k1
make (Rep r) beg fin nxt = (((beg,nxt,Eps):((nxt+1),nxt,Eps):((nxt+1),fin,Eps):(beg,fin,Eps):ts),k)
                         where (ts,k) = make r nxt (nxt+1) (nxt+2)

-- Задача 8 -----------------------------------------
-- Передумова: Довільний цикл в НСА включає хоча б один не-Eps перехід.  
getFrontier :: State -> Automation -> [Transition]
getFrontier = undefined

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions = undefined

makeDA' :: Automation -> [State] -> [MetaState] -> [MetaTransition] 
                   -> (MetaState, [MetaState], [MetaTransition])
makeDA' = undefined  

makeDA :: Automation -> Automation
makeDA  = undefined
-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigure, re1, re2, re3, re4, re5 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Alt (Term 'a') Null) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

ndaFigure, nda1, nda2, nda3, nda4, nda5 :: Automation
daFigure, da1, da2, da3, da4, da5 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

