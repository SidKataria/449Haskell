import Data.List
import System.Exit
import System.Environment
import Text.Read
 
main = do
        getTuplesFromFile "input.txt"
 

getTuplesFromFile file1  = do
          contents1 <- readFile file1
          let lines1 = lines (contents1)

          let tuples = (getTuplesFromLinesRaw lines1)

          putStrLn ("Number of tuples " ++ show (length tuples))

{- Take an array of lines and return only those lines that start with "(" -}
getTuplesFromLinesRaw lineSet = filter (\x -> isPrefixOf "(" x) lineSet