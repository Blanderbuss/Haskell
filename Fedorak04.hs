{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.Char
  
type Name = String
type Attributes = [(Name, String)]
data XML = Text String | Element Name Attributes [XML]   
         deriving (Eq, Show)
type Stack = [XML]

-- Задача 1 -----------------------------------------
skipSpace :: String -> String
skipSpace [] = []
skipSpace s | (head s /= ' ') && (head s /= '\n') = s
            | otherwise = skipSpace(tail s)

-- Задача 2 -----------------------------------------
getAttribute :: String -> XML -> String
getAttribute _ (Text _) = ""
getAttribute s (Element _ a x) | length (filter( (==s).fst ) a) == 0 = head (map(getAttribute s)x)
                               | otherwise = snd (head (filter( ((==s).fst) ) a))

-- Задача 3 -----------------------------------------
getName :: XML -> Name
getName (Text _) = ""
getName (Element n _ _) = n

getChildren :: String -> XML -> [XML]
getChildren _ (Text _) = []
getChildren s (Element _ _ x) = filter((==s).getName) x

-- Задача 4 -----------------------------------------
getChild :: String -> XML -> XML
getChild _ (Text _) = Text ""
getChild s (Element n a x) | null x = Text ""
                           | otherwise = let l = getChildren s (Element n a x) in if null l then (Text "") else head l

-- Задача 5 -----------------------------------------
addChild :: XML -> XML -> XML
-- Передумова: другий аргумент - завжди побудований конструктором Element
addChild _ (Text _) = undefined
addChild (Text t) (Element n a x) = Element n a (x ++ [(Text t)])
addChild (Element n0 a0 x0) (Element n a x) = Element n a (x ++ [(Element n0 a0 x0)])

-- Задача 6 -----------------------------------------
getValStr :: XML -> String
getValStr (Text t) = t
getValStr (Element _ _ x) = concatMap getValStr x

getValue :: XML -> XML
getValue (Text t) = Text t
getValue (Element n a x) = Text (getValStr (Element n a x))

-- Задача 7 -----------------------------------------
addText :: String -> Stack -> Stack
-- Передумова: Є по крайній мірі один елемент Element в стеку
addText "" st = st
addText s st = (addChild (Text s) (head st)):(tail st)

-- Задача 8 -----------------------------------------
popAndAdd :: Stack -> Stack
-- Передумова: Є по крайній мірі два елемента Elements в стеку
popAndAdd [] = []
popAndAdd (x:xs) = (addChild x (head(xs))):tail(xs)
 
-- Початковий елемент стеку 
sentinel :: XML
sentinel = Element "" [] []  

-- Задача 9 -----------------------------------------

selArg :: String -> (Name, String)
selArg s = let (f1,sn1) = break (== '\"') s
               (f2,_) = break (== '\"') (tail sn1) in (fst(parseName(skipSpace f1)),f2)

selArgs :: String -> [(Name, String)]
selArgs s = let arg = selArg s 
                rest = tail(dropWhile (/= '\"') (tail(dropWhile (/= '\"') s))) in if skipSpace rest == "" then [arg] else arg:(selArgs rest)

parseAttributes :: String -> (Attributes, String)
-- Передумова: Рядок, що містить XML-атрибути, синтаксично вірний
parseAttributes s = let t = break (=='>') s in if null (fst t) then ([],snd t) else ((selArgs (fst t)),(snd t))

-- Аналіз імені елемента/атрибута
parseName :: String -> (Name, String)
parseName []
  = error "Error: attempt to read empty name"
parseName s@(c1 : _)
  | isAlpha c1 = break (not . isNameChar) s
  | otherwise = error ("parseName error: name " ++ show s ++
                      " must begin with a letter")
  where
    isNameChar c = isAlpha c || isDigit c || elem c "-."

-- Задача 10 -----------------------------------------
parse :: String -> XML
-- Передумова: рядок, що містить XML-документ, синтаксично вірний
parse s = parse' (skipSpace s) [sentinel]

parse' :: String -> Stack -> XML
parse' "" ((Element _ _ x):_) = head x
parse' (hs:ts) st | (hs == '<') && ((head ts) == '/') = parse' (dropWhile (/='>') ts) (popAndAdd st)
                         | (hs == '<') = let (n,rest) = parseName(skipSpace ts)  
                                             (atrs,txt) = parseAttributes rest in parse' txt ((Element n atrs []):st)
                         | otherwise = parse' (dropWhile (/='<') ts) (addText (fst(break (=='<') ts)) st )


-----------------------------------------------------------------------
-- Деякі корисні функції перетворення в рядок і виводу
-- Функція перетворення в рядок ('show' function) для XML об'єктів
showXML :: XML -> String
showXML (Text t) = t
showXML (Element n as es)
     = "<" ++ n ++ showAtts as ++ ">" ++ concatMap showXML es ++ "</" ++ n ++ ">"
       where
          showAtts ast = concatMap showAtt ast
          showAtt (n1, v) = " " ++ n1 ++ "=" ++ "\"" ++ v ++ "\""
-- Функція перетворення в рядок ('show' function) для списку XML об'єктів
showXMLs :: [XML] -> String
showXMLs = concatMap showXML
-- Функція виводу XML об'єкта на екран
printXML :: XML -> IO()
printXML = putStrLn . showXML

-------------------------------------------------------------------------
-- Тестові дані
-- Прості тести XML-об'єктів (без проміжків)
s1, s2, s3 :: String
s1 = "<a>A</a>"
s2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
s3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"
-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

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

-- XML-документ з Мал.1
films :: String
films
  = "<filmlist>\n\
    \  <film title = \"Rear Window\">\n\
    \    <director>Alfred Hitchcock</director>\n\
    \    <composer>Franz Waxman</composer>\n\
    \    <year>1954</year>\n\
    \  </film>\n\
    \  <film   title =  \"2001: A Space Odyssey\">\n\
    \    <director>Stanley Kubrick</director>\n\
    \    <composer>Richard Strauss</composer>\n\
    \    <composer>Gyorgy Ligeti</composer>\n\
    \    <composer>Johann Strauss</composer>\n\
    \    <year>1968</year>\n\
    \  </film>\n\
    \  <film title=\"Lawrence of Arabia\"  >\n\
    \    <duration>228</duration>\n\
    \    <director>David Lean</director>\n\
    \    <composer>Maurice Jarre</composer>\n\
    \  </film>\n\
    \</filmlist>\n\n\n"

-- Результат аналізу  попереднього докуменнту ('parse films')
filmsParsed :: XML
filmsParsed
  = Element "filmlist" 
            [] 
            [Text "\n  ",
             Element "film" [("title","Rear Window")]
                            [Text "\n    ",
                             Element "director" [] [Text "Alfred Hitchcock"],
                             Text "\n    ",
                             Element "composer" [] [Text "Franz Waxman"],
                             Text "\n    ",
                             Element "year" [] [Text "1954"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","2001: A Space Odyssey")] 
                            [Text "\n    ",
                             Element "director" [] [Text "Stanley Kubrick"],
                             Text "\n    ",
                             Element "composer" [] [Text "Richard Strauss"],
                             Text "\n    ",
                             Element "composer" [] [Text "Gyorgy Ligeti"],
                             Text "\n    ",
                             Element "composer" [] [Text "Johann Strauss"],
                             Text "\n    ",
                             Element "year" [] [Text "1968"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","Lawrence of Arabia")] 
                            [Text "\n    ",
                             Element "duration" [] [Text "228"],
                             Text "\n    ",
                             Element "director" [] [Text "David Lean"],
                             Text "\n    ",
                             Element "composer" [] [Text "Maurice Jarre"],
                             Text "\n  "],
             Text "\n"]


