{-# OPTIONS_GHC -Wall #-}
module Fedorak07 where

-- import Data.List
type Index = Int
data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Index, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Index, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

getIndex :: BDDNode -> Index
getIndex (_,(i,_,_)) = i

getFalse :: BDDNode -> NodeId
getFalse (_,(_,f,_)) = f

getTrue :: BDDNode -> NodeId
getTrue (_,(_,_,t)) = t

-- Задача 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat (sid, bddnodes) en | null (filter ((==sid).fst) bddnodes) = False
                            | null (filter ((==(getIndex start)).fst) en) = False
                            | (next == 1) = True
                            | (next == 0) = False
                            | otherwise = checkSat (next,bddnodes) en
                            where start = head (filter ((==sid).fst) bddnodes)
                                  cond = snd (head (filter ((==(getIndex start)).fst) en))
                                  next = if cond then (getTrue start) else (getFalse start)

-- Задача 2 -----------------------------------------
sat :: BDD -> [[(Index, Bool)]]
sat (_,[]) = []
sat (sid, bddnodes) = filter (checkSat (sid,bddnodes)) (falseList ++ trueList)
                      where start = head (filter ((==sid).fst) bddnodes)
                            falseList | (getFalse start == 1) || (getFalse start == 0) = [[((getIndex start),False)]]
                                      | otherwise = map (((getIndex start),False):)(sat (getFalse start,bddnodes))
                            trueList  | (getTrue start == 1) || (getTrue start == 0) = [[((getIndex start),True)]]
                                      | otherwise = map (((getIndex start),True):)(sat (getTrue start,bddnodes))

-- Задача 3 -----------------------------------------
simplify :: BExp -> BExp
simplify (Not (Not (Prim b))) = Prim b
simplify (Not (Prim True)) = Prim False
simplify (Not (Prim False)) = Prim True
simplify (And (Prim True) (Prim b)) = Prim b
simplify (And (Prim False) (Prim _)) = Prim False
simplify (Or (Prim _) (Prim True)) = Prim True
simplify (Or (Prim b) (Prim False)) = Prim b
simplify be = be

-- Задача 4 -----------------------------------------
restrict :: BExp -> Index -> Bool -> BExp
restrict (Prim b) _ _= Prim b
restrict (IdRef i) i0 b | (i == i0) = Prim b
                        | otherwise = IdRef i
restrict (Not be) i0 b = simplify (Not (restrict be i0 b))
restrict (And be1 be2) i0 b = simplify (And (restrict be1 i0 b) (restrict be2 i0 b))
restrict (Or be1 be2) i0 b = simplify (Or (restrict be1 i0 b) (restrict be2 i0 b))

-- Задача 5 -----------------------------------------
-- Передумова: Кожна змінна (індекс) в бульовому виразі (BExp) з"являється 
--    точно один раз в списку індексів (Index); немає інших елементів
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs = buildBDD' e 2 xs 

buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Prim b) _ [] | b = (1, [])
                        | otherwise = (0, [])
buildBDD' be ni xs = (ni, [node] ++ falseTree ++ trueTree)
                   where cur = head xs
                         node = (ni, (cur,falseId,trueId))
                         (falseId,falseTree) = buildBDD' (restrict be cur False) (ni*2) (tail xs)
                         (trueId,trueTree) = buildBDD' (restrict be cur True) (ni*2+1) (tail xs)
                         --bf = if (restrict be cur False) == (Prim True) then 1 else 0
                         --bt = if (restrict be cur True) == (Prim True) then 1 else 0

-- Задача 6 -----------------------------------------
-- Передумова: Кожна змінна (індекс) в бульовому виразі (BExp) з"являється 
--    точно один раз в списку індексів (Index); немає інших елементів
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD e xs = transform (buildBDD e xs)

transform :: BDD -> BDD
transform bdd | not (null cps) = transform ((fst bdd),(map (tempRep (orig,cps)) newTree))
              | not (null tf) = transform (sameRes bdd (head tf))
              | otherwise = bdd
              where (orig, cps) = same bdd
                    newTree = orig:[x | x<-(snd bdd), (notElem (snd x) raw)]
                    raw = map snd cps
                    tf = [x | x<-(snd bdd), (getTrue x)==(getFalse x)]

sameRes :: BDD -> BDDNode -> BDD
sameRes bdd node | (getTrue node) == (getFalse node) = ((fst bdd),map (tempRes node) (filter (/=(node)) (snd bdd)))
                 | otherwise = bdd

tempRes :: BDDNode -> BDDNode -> BDDNode
tempRes (i0,(_,f0,_)) (i,(v,f,t)) | (f==i0) && (t==i0) = (i,(v,f0,f0))
                                  | (f==i0) = (i,(v,f0,t))
                                  | (t==i0) = (i,(v,f,f0))
                                  | otherwise = (i,(v,f,t))

same :: BDD -> (BDDNode,[BDDNode])
same (_, []) = ((0,(0,0,0)),[])
same (f, (x:xs)) | null cps = same (f, xs)
                 | otherwise = (x,cps)
                 where cps = filter ((==(snd x)).snd) xs

tempRep :: (BDDNode,[BDDNode]) -> BDDNode -> BDDNode
tempRep ((i0,_),cps) (i,(v,f,t)) | (elem f oldInd) && (elem t oldInd) = (i,(v,i0,i0))
                                 | (elem f oldInd) = (i,(v,i0,t))
                                 | (elem t oldInd) = (i,(v,f,i0))
                                 | otherwise = (i,(v,f,t))
                                 where oldInd = map fst cps

------------------------------------------------------
-- Приклади для тестування..

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))
b9 = And (IdRef 3) (Or (IdRef 2) (And (Not (IdRef 2)) (IdRef 1)))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])
bdd9 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,0,0)),(9,(3,0,1)),(5,(2,10,11)),(10,(3,0,1)),(11,(3,0,1))])



