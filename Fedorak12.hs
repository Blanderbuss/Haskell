{-# OPTIONS_GHC -Wall #-}
module Fedorak12 where

import Data.Maybe

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show, Ord)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Show, Ord) 

type TypeTable = [(String, Type)]
type TEnv      = TypeTable    -- тобто [(String, Type)]
type Sub       = TypeTable    -- тобто [(String, Type)]  

-- Задача 1 -----------------------------------------
-- Передумова: Єлемент, що шукається, є в таблиці
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp e l = snd $ head $ filter ((==e).fst) l

-- Задача 2 -----------------------------------------
tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp e d l | null res = d
                  | otherwise = snd $ head res
                  where res = filter ((==e).fst) l

-- Задача 3 -----------------------------------------
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp e l = map fst $ filter ((==e).snd) l

-- Задача 4 -----------------------------------------
occurs :: String -> Type -> Bool
occurs _ TInt = False
occurs _ TBool = False
occurs _ TErr = False
occurs v (TVar x) = x == v
occurs v (TFun t1 t2) = occurs v t1 || (occurs v t2)

-- Задача 5 -----------------------------------------
-- Передумова: Немає функцій визначених користувачем (конструктор Fun)
-- Передумова: Всі змінні типів (з виразів) мають зв"язування в середовищі типів 
inferType :: Expr -> TEnv -> Type
inferType (Number _) _ = TInt
inferType (Boolean _) _ = TBool
inferType (Id s) en = tryToLookUp s TErr en
inferType (Prim s) _ = tryToLookUp s TErr primTypes
inferType (Cond c t f) en | ct == TErr || not (isBool ct) || tt /= ft = TErr
                          | otherwise = tt
                          where ct = inferType c en
                                tt = inferType t en
                                ft = inferType f en
inferType (App f a) en | not (isFun temp) = TErr
                       | inferType a en == i = o
                       | otherwise = TErr
                        where temp = inferType f en
                              (TFun i o) = temp
inferType (Fun _ e) en = inferType e en

isBool ::Type -> Bool
isBool TBool = True
isBool _ = False

isFun :: Type -> Bool
isFun (TFun _ _) = True
isFun _ = False

-- Задача 6 -----------------------------------------
applySub :: Sub -> Type -> Type
applySub _ TInt = TInt
applySub _ TBool = TBool
applySub su (TFun i o) = TFun (applySub su i) (applySub su o)
applySub su (TVar s) = tryToLookUp s (TVar s) su
applySub _ TErr = TErr 

-- Задача 7 -----------------------------------------
unify :: Type -> Type -> Maybe Sub
unify t t' = unifyPairs [(t, t')] []

unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs [] su = Just su
unifyPairs ((TInt, TInt):xt) su = unifyPairs xt su
unifyPairs ((TBool, TBool):xt) su = unifyPairs xt su
unifyPairs (((TVar v1), (TVar v2)):xt) su | v1 == v2 = unifyPairs xt su
unifyPairs (((TVar v),t):xt) su | occurs v t = Nothing
                                | otherwise = unifyPairs (map (pairSub [(v,t)]) xt) ((v,t):su)
unifyPairs ((t,(TVar v)):xt) su | occurs v t = Nothing
                                | otherwise = unifyPairs (map (pairSub [(v,t)]) xt) ((v,t):su)
unifyPairs (((TFun t1 t2), (TFun t3 t4)):xt) su = unifyPairs ((t1,t3):((t2,t4):xt)) su
unifyPairs _ _ = Nothing

pairSub :: Sub -> (Type, Type) -> (Type, Type)
pairSub su (t1,t2) = ((applySub su t1), (applySub su t2))

-- Задача 8 -----------------------------------------
inferPolyType :: Expr -> Type
inferPolyType e =
    let vx   = ['a' : show n | n <- [(1::Int)..]]
        (_, t, _) = inferPolyType' e [] vx    
    in t  

inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String])
inferPolyType' (Number _) _ nx = ([], TInt, nx)
inferPolyType' (Boolean _) _ nx = ([], TBool, nx)
inferPolyType' (Id s) en nx = ([], (tryToLookUp s TErr en), nx)
inferPolyType' (Prim s) _ nx = ([], (tryToLookUp s TErr primTypes), nx)
inferPolyType' (Fun x e) en (vs:vx) | isErr te = ([],TErr,[])
                                    | otherwise = (nsu, (TFun (applySub nsu (TVar vs)) te), nv)
                                    where sub = (x,(TVar vs)):en
                                          (nsu, te, nv) = inferPolyType' e sub vx
inferPolyType' (App f e) en (vs:vx) | isNothing mnsu = ([], TErr, [])
                                    | otherwise = (combineSubs [ssu,nsu,fsu],applySub nsu (TVar vs),sv)
                                    where (fsu,ft,fv) = inferPolyType' f en vx
                                          (ssu,te,sv) = inferPolyType' e (updateTEnv en fsu) fv
                                          mnsu = unify ft (TFun te (TVar vs))
                                          (Just nsu) = mnsu
inferPolyType' (Cond c t f) en (_:vx) = (combineSubs[csu,tsu,fsu],tt,fv)
                                     where (csu,_,cv) = inferPolyType' c en vx
                                           (tsu,tt,tv) = inferPolyType' t csu cv
                                           (fsu,_,fv) = inferPolyType' f csu tv
inferPolyType' _ _ _ = ([],TErr,[])




isErr :: Type -> Bool
isErr TErr = True
isErr _ = False

--------------------------------------------------------------------
showT :: Type -> String
showT TInt        = "Int"
showT TBool       = "Bool"
showT (TFun t t') = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a)    = a
showT TErr        = "Type error"

-- Типи базових операцій (примітивів)...
primTypes :: TypeTable
primTypes 
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

---------------------------------------------------
-- Допоміжні функції 
updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub = map modify tenv
  where modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld = sNew ++ updateTEnv sOld sNew

-- В combineSubs [s1, s2,..., sn], s1 повинна бути *самою останньою* 
-- підстановкою і повинна бути застосована *заключною*
combineSubs :: [Sub] -> Sub
combineSubs = foldr1 combine

------------------------------------------------------
-- Вивод мономорфного типу - приклади тестів...
env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Приклади для тестування уніфікації (unify)...
u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Вивод поліморфного типу - приклади тестів...
ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3"))) 
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) 
              (TFun (TVar "a2") (TVar "a3"))
