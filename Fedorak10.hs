{-# OPTIONS_GHC -Wall #-}
module Fedorak10 where

import Text.ParserCombinators.Parsec

-- Çàäà÷à 1 -----------------------------------------
evExpr  :: String -> Maybe Integer
evExpr s = case parse pevExpr "" s of
              Right ex -> Just $ read ex
              Left _ -> Nothing

pevExpr :: Parser String
pevExpr = do {n <- parens number;return $ show n}

sign :: Parser String 
sign = string "-" <|> pure "" 

reserved :: String -> Parser ()
reserved s = do { _ <- string s; spaces} 

lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces ; return a}

parens :: Parser a -> Parser a 
parens p = do reserved "(" 
              n <- lexem p 
              reserved ")" 
              return n

number :: Parser Int
number = do s <- sign 
            cs <- many1 digit
            return $ read (s ++ cs) 



-- Çàäà÷à 2 -----------------------------------------
data Expr = Add Expr Expr | Sub Expr Expr
          | Mul Expr Expr | Mod Expr Expr | Div Expr Expr
          | Var String | Lit Int
            deriving (Show, Eq)

fullExpr :: Parser Expr
fullExpr = do spaces;
              ex <- expr 
              eof 
              return ex  

astExpr :: String -> Maybe Expr
astExpr str = case (parse fullExpr "" str) of
               Left _     -> Nothing
               Right expr -> Just expr

int :: Parser Expr
int = do { n <-  lexem number; return (Lit n)}

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop, mulop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)
mulop = (infixOp "*" Mul) <|> (infixOp "%" Mod) <|> (infixOp "/" Div)

expr, term, factor :: Parser Expr
expr   = term `chainl1` addop
term   = factor `chainl1` mulop
factor = int <|> parens expr <|> iden

iden :: Parser Expr
iden = do{n <- letter; m <- many(letter<|>digit);spaces; return $ Var $ n:m } 


-- Çàäà÷à 3 -----------------------------------------
data RE = Null           | -- Íóëü âèðàç
          Term Char  | -- Òåðì³íàëüíèé ñèìâîë
          Seq RE RE | -- Ïîñë³äîâí³ñòü
          Alt RE RE  | -- Àëüòåðíàòèâà
          Rep RE       | -- Ïîâòîðåííÿ (*)
          Plus RE      | -- Ïîâòîðåííÿ (+)
          Opt RE        -- Íåîáîâ’ÿçêîâå âõîäæåííÿ (?)
       deriving (Eq, Show) 

reg :: Parser RE
reg = do spaces;
         rex <- rexpr 
         eof 
         return rex  

symbol :: Char ->  Parser ()
symbol ch = lexem (char ch >> return ())

rexpr,rterm,rfact :: Parser RE
rexpr = rterm `chainl1` topp
rterm = (try(seq' rfact rterm)) <|> rfact
rfact = symbol <|> parens rexpr

topp,bott :: Parser (RE -> RE -> RE)
topp = undefined
bott = infixOp "|" Alt

regExp :: String -> Maybe RE
regExp str = case (parse reg "" str) of
               Left _   -> Nothing
               Right rg -> Just rg

-- Çàäà÷à 4 -----------------------------------------			   
type Name = String
type Attributes = [(String, String)]
data XML  =  Text String | Element Name Attributes [XML]
          deriving (Eq, Show)

fullXML  :: Parser XML 
fullXML = undefined

anXML :: String -> Maybe XML
anXML str = case (parse fullXML "" str) of
               Left _    -> Nothing
               Right xml -> Just xml

------------------------------------------------------
re1, re2, re3, re4, re5 :: RE
re1  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2  = Seq (Term 'x') (Rep (Term '\''))
re3  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4  = Seq (Alt (Term 'a') Null) (Term 'a')
re5  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]