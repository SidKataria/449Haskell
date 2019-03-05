import Data.Char
import Data.List
import System.IO
import System.FilePath
import System.Environment
import System.Exit


--Trimming Strings
trim :: String -> String
trim = x . x
   where x = reverse . dropWhile isSpace

--error function  
errorFound :: Handle -> FilePath -> [Char] -> IO b0
errorFound inHandle filename str = do
  putStrLn "Error found"
  outHandle <- openFile filename WriteMode
  hPutStrLn outHandle (str)
  hClose outHandle
  hClose inHandle
  exitFailure

  --Strings to int method
intString :: (Integral b) => [Char] -> [b]
intString "" = []
intString other = (whichChar (other!!1)):[(whichChar (other!!3))]

--Char to int returner
whichChar :: (Integral b) => Char -> b
whichChar x
  | x == 'A' = 1
  | x == 'B' = 2
  | x == 'C' = 3
  | x == 'D' = 4
  | x == 'E' = 5
  | x == 'F' = 6
  | x == 'G' = 7
  | x == 'H' = 8
  | x == '1' = 1
  | x == '2' = 2
  | x == '3' = 3
  | x == '4' = 4
  | x == '5' = 5
  | x == '6' = 6
  | x == '7' = 7
  | x == '8' = 8
  | otherwise = -1
  
--String in Integer
whichString :: [Char] -> [Integer]
whichString "" = []
whichString other = [read other :: Integer]
  
  
--Main function
main = do

  --Test print
  putStrLn "Just started main" 
  
  args <- getArgs
  handle <- openFile (head args) ReadMode
  --outHandle <- openFile (last args) WriteMode
  
  contents <- hGetContents handle
  
  --Test printing the contents to terminal
  putStrLn contents
  
  --Test printing contents to output file
  outHandle <- openFile (last args) WriteMode
  hPutStrLn outHandle ("The file is: \n"++contents)
  hClose outHandle
  
  --Test print
  putStrLn "Contents gottem"
  
  --Manipulating contents
  let input = lines contents
  
  let givenLines = [trim rLines | rLines <- givenLines]
  
  --Finding Index Of  "Name:" header
  let x = elemIndex "name:" givenLines
  let iName = case x of Nothing -> -1
                        Just a -> a
 
  --Finding Index Of  "forced partial assignment:" header
  let x = elemIndex "forced partial assignment:" givenLines
  let iFPA = case x of Nothing -> -1
                       Just a -> a

  --Finding Index Of  "forbidden machine:" header
  let x = elemIndex "fobidden machine:" givenLines
  let iFM = case x of Nothing -> -1
                      Just a -> a
  
  --Finding Index Of  "too-near tasks:" header
  let x = elemIndex "too-near tasks:" givenLines
  let iTNT = case x of Nothing -> -1
                       Just a -> a
  
  --Finding Index Of  "machine penalties:" header	
  let x = elemIndex "machine penalties:" givenLines
  let iMP = case x of Nothing -> -1
                      Just a -> a
  
  --Finding Index Of  "too-near penalities" header	
  let x = elemIndex "too-near penalities" givenLines
  let iTNP = case x of Nothing -> -1
                       Just a -> a

  --Test print
  putStrLn "Just before the if statement"
  
  --If Headers not found: ERROR (LOOPING)
  if ((iName == -1)||(iFPA == -1)||(iFM == -1)||(iTNT == -1)||(iMP == -1)||(iTNP == -1)) 
    then (errorFound handle (last args) "Error while parsing input file") else (putStrLn "")
  
  --Test print
  putStrLn "Here now"
  
  --Strings data
  let sName = take (iFPA-(iName+1)) givenLines
  let sFPA = take (iFM-(iFPA+1)) (drop (iFPA+1) givenLines)
  let sFM = take (iTNT-(iFM+1)) (drop (iFM+1) givenLines)
  let sTNT = take (iMP-(iTNP+1)) (drop (iTNT+1) givenLines)
  let sMP = take (iTNP-(iMP+1)) (drop (iMP+1) givenLines)
  let sTNP = drop (iTNP+1) givenLines
  
  --Test print
  putStrLn "Here now 2"
  
  --data structure for integers
  let intFPA = [intString stringFPA | stringFPA <- sFPA, (length stringFPA) > 0]
  
  let intFM = [intString stringFM | stringFM <- sFM, (length stringFM) > 0]
  
  let intTNT = [intString stringTNT | stringTNT <- sTNT, (length stringTNT) > 0]
  
  let intMP = [intString stringMP | stringMP <- sMP, (length stringMP) > 0]
  
  let intTNP = [intString stringTNP | stringTNP <- sTNP, (length stringTNP) > 0]
  
  print intFPA
  
  --Closing handles
  --hClose outHandle
  hClose handle