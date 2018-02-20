module HTest where

stringToInt :: Int -> String -> Maybe Integer
stringToInt n s | n <= 0 || n > 16 = Nothing
                | null s = Nothing
                | otherwise = parser n (reverse s)

parser :: Int -> String -> Maybe Integer
parser n [] = Just 0
parser n (x:xs) | next == Nothing = Nothing
                | x == '0' && n /= 1 = Just $ ni*nn
                | x == '1' && n >= 1 = Just $ 1 + ni*nn
                | x == '2' && n >= 3 = Just $ 2 + ni*nn
                | x == '3' && n >= 4 = Just $ 3 + ni*nn
                | x == '4' && n >= 5 = Just $ 4 + ni*nn
                | x == '5' && n >= 6 = Just $ 5 + ni*nn
                | x == '6' && n >= 7 = Just $ 6 + ni*nn
                | x == '7' && n >= 8 = Just $ 7 + ni*nn
                | x == '8' && n >= 9 = Just $ 8 + ni*nn
                | x == '9' && n >= 10 = Just $ 10 + ni*nn
                | x == 'a' && n >= 11 = Just $ 11 + ni*nn
                | x == 'b' && n >= 12 = Just $ 12 + ni*nn
                | x == 'c' && n >= 13 = Just $ 13 + ni*nn
                | x == 'd' && n >= 14 = Just $ 14 + ni*nn
                | x == 'e' && n >= 15 = Just $ 15 + ni*nn
                | x == 'f' && n >= 16 = Just $ 16 + ni*nn
                | otherwise = Nothing
                where next = parser n xs
                      (Just ni) = next
                      nn = toInteger n

