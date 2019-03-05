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
  outHandle <- openFile filename WriteMode
  hPutStrLn outHandle (str)
  hClose outHandle
  hClose inHandle
  exitFailure
  
--nop else function for IO
nop :: IO ()
nop = sequence_ []
  
--Main function
main = do
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

  --If Headers not found: ERROR
  if ((iName == -1)||(iFPA == -1)||(iFM == -1)||(iTNT == -1)||(iMP == -1)||(iTNP == -1)) then (errorFound handle (last args) "Error while parsing input file") else (readFile (head args))
  
  --Strings data
  let sName = take (iFPA-(iName+1)) givenLines
  let sFPA = take (iFM-(iFPA+1)) (drop (iFPA+1) givenLines)
  let sFM = take (iTNT-(iFM+1)) (drop (iFM+1) givenLines)
  let sTNT = take (iMP-(iTNP+1)) (drop (iTNT+1) givenLines)
  let sMP = take (iTNP-(iMP+1)) (drop (iMP+1) givenLines)
  let sTNP = drop (iTNP+1) givenLines
  
  
  --Closing handles
  --hClose outHandle
  hClose handle