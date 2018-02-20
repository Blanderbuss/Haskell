{-# OPTIONS_GHC -Wall #-}
module HW05 where

data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Show, Eq)

-- Задача 1 -----------------------------------------
instance Ord AbstractInteger where
   (<=) Zero Zero = True
   (<=) Zero (Succ _) = True
   (<=) Zero (Pred _) = False
   (<=) (Pred _) (Succ _) = True
   (<=) (Pred _) Zero = True
   (<=) (Pred ai1) (Pred ai2) = (ai1 <= ai2)
   (<=) (Succ _) Zero = False
   (<=) (Succ _) (Pred _) = False
   (<=) (Succ ai1) (Succ ai2) = (ai1 <= ai2)
   
-- Задача 2 ----------------------------------------
aiToInteger :: AbstractInteger -> Integer
aiToInteger Zero = 0
aiToInteger (Pred ai) = (aiToInteger ai) - 1
aiToInteger (Succ ai) = (aiToInteger ai) + 1
 
-- Задача 3 -----------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs Zero Zero = Zero
plusAbs Zero (Succ ai) = Succ ai
plusAbs Zero (Pred ai) = Pred ai
plusAbs (Pred ai1) (Succ ai2) = plusAbs ai1 ai2
plusAbs (Pred ai) Zero = Pred ai
plusAbs (Pred ai1) (Pred ai2) = Pred (Pred (plusAbs ai1 ai2))
plusAbs (Succ ai) Zero = Succ ai
plusAbs (Succ ai1) (Pred ai2) = plusAbs ai1 ai2
plusAbs (Succ ai1) (Succ ai2) = Succ (Succ (plusAbs ai1 ai2))

-- Задача 4 -----------------------------------------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs Zero _ = Zero
timesAbs _ Zero = Zero

timesAbs (Pred Zero) (Pred Zero) = Succ Zero
timesAbs (Succ Zero) (Pred Zero) = Pred Zero
timesAbs (Pred Zero) (Succ Zero) = Pred Zero
timesAbs (Succ Zero) (Succ Zero) = Succ Zero

timesAbs (Succ ai) (Pred Zero) = Pred (timesAbs ai (Pred Zero))
timesAbs (Pred Zero) (Succ ai) = Pred (timesAbs ai (Pred Zero))
timesAbs (Pred ai) (Pred Zero) = Succ (timesAbs ai (Pred Zero))
timesAbs (Pred Zero) (Pred ai) = Succ (timesAbs ai (Pred Zero))

timesAbs ai (Succ Zero) = ai
timesAbs (Succ Zero) ai = ai

timesAbs (Pred ai1) (Pred ai2) = timesAbs (timesAbs (Pred ai1) (Pred Zero )) (timesAbs (Pred ai2) (Pred Zero ))
timesAbs (Pred ai1) (Succ ai2) = (Pred ai1) + (timesAbs (Pred ai1) ai2)
timesAbs (Succ ai1) (Pred ai2) = (Pred ai2) + (timesAbs (Pred ai2) ai1)
timesAbs (Succ ai1) (Succ ai2) = (Succ ai1) + (timesAbs (Succ ai1) ai2)

-- Задача 5 -----------------------------------------
instance Num AbstractInteger  where
    (+)   = plusAbs
    (*)   = timesAbs
    negate ai   = timesAbs ai (Pred Zero)
    fromInteger i | i == 0 = Zero
                  | i > 0 = Succ (fromInteger (i-1))
                  | otherwise = Pred (fromInteger (i+1))
    abs Zero      = Zero
    abs (Succ ai) = Succ ai
    abs (Pred ai) = negate ai
    signum Zero      = Zero
    signum (Succ _) = Succ Zero
    signum (Pred _) = Pred Zero

-- Задача 6 -----------------------------------------
factorial :: (Eq a, Num a) => a -> a
factorial a | (a * (a-1)) == a = a
            | otherwise = a * factorial(a-1)

-- Задача  7 -----------------------------------------
data Quaternion = Quaternion Double Double Double Double deriving (Eq)

sign :: Double -> String
sign d | d >= 0 = "+"
       | otherwise = "-"

instance Show Quaternion where
    show (Quaternion d1 d2 d3 d4) = (show d1) ++ (sign d2) ++ (show (abs d2)) ++ "i" ++ (sign d3) ++ (show (abs d3)) ++ "j" ++ (sign d4) ++ (show (abs d4)) ++ "k"

-- Задача 8 -----------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion (Quaternion l1 i1 j1 k1) (Quaternion l2 i2 j2 k2) = Quaternion (l1+l2) (i1+i2) (j1+j2) (k1+k2)

-- Задача 9 -----------------------------------------
timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion (Quaternion l1 i1 j1 k1) (Quaternion l2 i2 j2 k2) = Quaternion (l1*l2-i1*i2-j1*j2-k1*k2) (l1*i2+i1*l2+j1*k2-k1*j2) (l1*j2-i1*k2+j1*l2+k1*i2) (l1*k2+i1*j2-j1*i2+k1*l2)

--- Задача 10 ----------------------------------------
instance Num Quaternion  where
    (+)   = plusQuaternion
    (*)   = timesQuaternion
    negate (Quaternion l i j k) = Quaternion (-l) (-i) (-j) (-k)
    fromInteger i = Quaternion (fromInteger i) 0 0 0
    abs (Quaternion l i j k) = Quaternion (sqrt(l*l+i*i+j*j+k*k)) 0 0 0
    signum (Quaternion l i j k) = let (Quaternion l0 _ _ _) = abs (Quaternion l i j k) in (Quaternion (l/l0) (i/l0) (j/l0) (k/l0))

