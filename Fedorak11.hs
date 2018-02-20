{-# OPTIONS_GHC -Wall #-}
module Fedorak11 where

import Data.Maybe

-- Всі програми і їх елементи являються вірними (well-formed) в наступному значенні:
--   Всі оператори, функції і процедури застосовуються  
--      до вірної кількості аргументів, кожний з яких має відповідний тип.
--   Вирази, які повинні обчислювати логічні значення, будуть завжди обчислюватися
--     до 0 (false) або 1 (true).
--   В присвоєнні масиву  a[i] = e масив завжди визначений (в області дії).
--   Процедура, що викликається як x := p(e1, …, en), завжди буде повертати значення 
--     (закінчує своє обчислення оператором return e) 
--   Оператор return завжди останній оператор для виконання в блоку процедури 
--     (тобто немає "мертвого коду")

--------------------------------------------------------------------
type Id = String
data Value = I Int | A [(Int, Int)]
           deriving (Eq, Show)
data Op = Add | Minus | Mul | Less | Equal | Index
          deriving (Eq, Show)
data Exp = Const Value | 
           Var Id | 
           OpApp Op Exp Exp |
           Cond Exp Exp Exp |
           FunApp Id [Exp] 
         deriving (Eq, Show)

data VarDef  =  Arr Id  | Int Id  
               deriving (Eq, Show)
type FunDef  =  (Id, ([VarDef], Exp))

data Scope = Local | Global
           deriving (Eq, Show)
type Binding = (Id, (Scope, Value))
type State = [Binding]

data Statement = Assign Id Exp |
                 AssignA Id Exp Exp |
                 If Exp Block Block |
                 While Exp Block |
                 Call Id Id [Exp] |
                 Return Exp 
               deriving (Eq, Show)

type Block     = [Statement]
type ProcDef   = (Id, ([VarDef], Block))
type Program   = ([VarDef], [FunDef], [ProcDef])

-- Задача 1 -----------------------------------------
getValue :: Id -> State -> Value
-- Передумова: Значення змінної Id є в стані State
getValue x t = snd $ lookUp x t 

-- Задача 2 -----------------------------------------
getLocals :: State -> State
getLocals s = filter ((==Local).fst.snd) s

getGlobals :: State -> State
getGlobals s = filter ((==Global).fst.snd) s

-- Задача 3 -----------------------------------------
assignArray :: Value -> Value -> Value -> Value
-- Аргументи - масив, індекс і (нове) значення відповідно
-- Передумова: Три аргумента (Value)  мають значення відповідного типу  
--     (масив (A),  ціле (I) і ціле (I)) відповідно.
assignArray (A arr) (I ind) (I val) = A $ (ind,val):(filter ((/=ind).fst) arr)
assignArray _ _ _ = undefined

-- Задача 4 -----------------------------------------
updateVar :: (Id, Value) -> State -> State
updateVar (i, val) st | ch == Nothing = (i, (Local, val)):st
                      | otherwise = changeBind (i,(sc,val)) st--(i,(sc,val)):(filter ((/=i).fst) st)
                       where ch = lookup i st
                             Just (sc,_) = ch

changeBind :: Binding -> State -> State
changeBind _ [] = []
changeBind (i,(sc,val)) ((i0,(sc0,val0)):xs) | i == i0 = (i,(sc,val)):xs
                                             | otherwise = (i0,(sc0,val0)):(changeBind (i,(sc,val)) xs)

-- Задача 5 -----------------------------------------
applyOp :: Op -> Value -> Value -> Value
-- Передумова: Значення мають відповідні типи (I або A) для кожної операції
applyOp Add (I i1) (I i2) = I $ i1+i2
applyOp Minus (I i1) (I i2) = I $ i1-i2
applyOp Mul (I i1) (I i2) = I $ i1*i2
applyOp Less (I i1) (I i2) = I $ if i1<i2 then 1 else 0
applyOp Equal (I i1) (I i2) = I $ if i1==i2 then 1 else 0
applyOp Index (A arr) (I i) = I $ if lookup i arr == Nothing then 0 else res
                            where Just res = lookup i arr
applyOp _ _ _ = undefined

-- Задача 6 -----------------------------------------
bindArgs :: [Id] -> [Value] -> State
-- Передумова: списки мають однакову довжину
bindArgs [] _ = []
bindArgs _ [] = []
bindArgs (i:is) (v:vs) = (i,(Local,v)):(bindArgs is vs)

-- Задача 7 -----------------------------------------
eval :: Exp -> [FunDef] -> State -> Value
eval (Const c) _ _ = c
eval (Var i) _ st = getValue i st
eval (Cond c t f) dfx st | eval c dfx st == I 1 = eval t dfx st
                         | eval c dfx st == I 0 = eval f dfx st
                         | otherwise = error ("expression " ++ (show c) ++ " is not predicate")
eval (OpApp op e1 e2) dfx st = applyOp op (eval e1 dfx st) (eval e2 dfx st)
eval (FunApp f es) dfx st = eval ef dfx $ binArgs as vs ++ st
                          where (_,(as,ef)) = head $ filter ((==f).fst) dfx
                                vs = evalArgs es dfx st


evalArgs :: [Exp] -> [FunDef] -> State -> [Value]
evalArgs [] _ _ = []
evalArgs (e:es) dfx st = (eval e dfx st):(evalArgs es dfx st)

binArgs :: [VarDef] -> [Value] -> State
binArgs [] _ = []
binArgs _ [] = []
binArgs ((Arr i):is) ((A v):vs) = (i,(Local,(A v))):(binArgs is vs)
binArgs ((Int i):is) ((I v):vs) = (i,(Local,(I v))):(binArgs is vs)
binArgs _ _ = undefined


-- Задача 8 -----------------------------------------
executeStatement :: Statement -> [FunDef] -> [ProcDef] -> State -> State
executeStatement (Assign i e) dfx _ st = updateVar (i, (eval e dfx st)) st
executeStatement (AssignA i ei ev) dfx _ st = updateVar (i, assignArray (getValue i st) (eval ei dfx st) (eval ev dfx st)) st
executeStatement (If e bt bf) dfx dpx st | eval e dfx st == I 1 = executeBlock bt dfx dpx st
                                         | eval e dfx st == I 0 = executeBlock bf dfx dpx st
                                         | otherwise = error ("expression " ++ (show e) ++ " is not predicate")
executeStatement (While e b) dfx dpx st | eval e dfx st == I 1 = executeStatement (While e b) dfx dpx (executeBlock b dfx dpx st)
                                        | eval e dfx st == I 0 = st
                                        | otherwise = error ("expression " ++ (show e) ++ " is not predicate")-- Call Id Id [Exp]
executeStatement (Call "" ip es) dfx dpx st = getLocals st ++ getGlobals (executeBlock bp dfx dpx (lcls ++ getGlobals st))
                                           where (_,(varl,bp)) = head $ filter ((==ip).fst) dpx
                                                 lcls = binArgs varl (evalArgs es dfx st)
executeStatement (Call iv ip es) dfx dpx st | lookup iv st == Nothing = (iv, (Local, getValue "$res" res)):(getLocals st) ++ getGlobals res
                                            | otherwise = (getLocals st) ++ getGlobals res
                                            where (_,(varl,bp)) = head $ filter ((==ip).fst) dpx
                                                  lcls = binArgs varl (evalArgs es dfx st)
                                                  res = executeBlock bp dfx dpx (lcls ++ getGlobals st)
executeStatement (Return e) dfx _ st = updateVar ("$res", eval e dfx st) st


{-evalPArgs :: [Exp] -> [FunDef] -> [ProcDef] -> State -> [Value]
evalPArgs [] _ _ = []
evalPArgs (e:es) dfx dpx st = (eval e dfx st):(evalPArgs es dfx st)-}

executeBlock :: Block -> [FunDef] -> [ProcDef] -> State -> State
executeBlock [] _ _ st = st
executeBlock (s:sx) dfx dpx st = executeBlock sx dfx dpx (executeStatement s dfx dpx st)

---------------------------------------------------------------------
-- Допоміжні функції і дані для тестування...
-- Функція пошуку...
lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t = fromMaybe (error ("\nСпроба знайти  " ++ show x ++ 
                      " в таблиці котра має лише звязування: " ++ 
                      show (map fst t))) 
              (lookup x t)

-- Стан для тестування
sampleState :: State
sampleState  = [("x", (Local, I 5)), ("y", (Global, I 2)), ("a", (Global, listToVal [4,2,7]))]

-- Перетворює список цілих в масив Value...
listToVal :: [Int] -> Value
listToVal xs  = A (zip [0..] xs)

 -- Перетворює ціле в Exp...
intToExp :: Int -> Exp
intToExp n
  = Const (I n)

-- Реалізація виконання програми 
program :: Program -> State 
program (dvx, dfx, dpx) = 
   let initv :: VarDef -> Binding
       initv (Arr v) = (v, (Global, A []))
       initv (Int v) = (v, (Global, I 0)) 
       state = map initv dvx 
       ( _, bl) = lookUp "main" dpx
   in  executeBlock bl dfx dpx state   

-- fib чиста функція
-- Функція fib, що обчислює число Фібоначчі
-- function  fib(integer n) =
--     (n < 3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib
  = ("fib",
     ([Int "n"], Cond (OpApp Less (Var "n") (Const (I 3)))
                  (Const (I 1))
                  (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const (I 1))])
                             (FunApp "fib" [OpApp Minus (Var "n") (Const (I 2))]))
     )
    )

-- Масив
sampleArray :: Exp
sampleArray 
  = Const (listToVal [9,5,7,1])

-- Сума елементів масиву 0..n ...
sumA1 :: ProcDef
sumA1
  = ("sumA1",
     ([Arr "a", Int "n"], 
                  [Assign "s" (Const (I 0)),
                   Assign "i" (Const (I 0)),
                   Assign "limit" (OpApp Add (Var "n") (Const (I 1))),
                   While (OpApp Less (Var "i") (Var "limit"))
                         [Assign "s" (OpApp Add (Var "s") 
                                                (OpApp Index (Var "a") (Var "i"))),
                          Assign "i" (OpApp Add (Var "i") (Const (I 1)))
                         ],
                   Return (Var "s")]
     )
    )

-- Додавання двох чисел...
gAdd :: ProcDef
gAdd
  = ("gAdd", 
     ([Int "x", Int "y"], [Assign "gSum" (OpApp Add (Var "x") (Var "y"))])
    )

-- Повна програма
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],[Call "" "gAdd" [intToExp 5, intToExp 10] ]))])