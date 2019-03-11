import Data.List
import System.Exit
import System.Environment
import Text.Read

main = do
        getTuplesFromFile inputFileName


getTuplesFromFile file1  = do
          contents1 <- readFile file1
          let lines1 = lines (contents1)

          let tuples = (getTuplesFromLinesRaw lines1)



          let outputTuples = foldl1 (\x y -> x ++ "\n" ++ y) tuples

          putStrLn ("Number of tuples " ++ show (length tuples))
          putStrLn outputTuples


getTuplesFromLinesRaw lineSet = filter (\x -> isPrefixOf "(" x) lineSet



inputFileName = "input.txt"