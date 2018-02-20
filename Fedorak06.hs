{-# OPTIONS_GHC -Wall #-}
module Main where
import System.Environment
--import System.IO

main :: IO ()
main = do
      args <- getArgs
      if (null args) then do
         x <- getContents 
         putStr (unlines (sortQuick (lines x)))
      else do
         x <- readFile (head args)
         putStrLn (unlines (sortQuick (lines x)))


sortQuick :: [String] -> [String]
sortQuick [] = []
sortQuick (x:xs) = let smaller = sortQuick [a | a <- xs, a<=x]
                       bigger = sortQuick [a | a <- xs, a>x]
                   in smaller ++ [x] ++ bigger