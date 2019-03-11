--CPSC 449
--Winter 2019
--Assignment 2
--Name: Siddharth Kataria
--UCID: 30000880 
{-
References:
http://learnyouahaskell.com/input-and-output
https://rosettacode.org/wiki/Category:Haskell
http://book.realworldhaskell.org/read/io.html
https://www.reddit.com/r/haskellquestions/
https://stackoverflow.com/questions/27196642/what-is-an-example-implementation-of-exit-code-for-haskell-program-that-returns
https://en.wikibooks.org/wiki/Haskell/Lists_and_tuples
https://wiki.haskell.org/Tutorials/Programming_Haskell/String_IO
https://en.wikibooks.org/wiki/Haskell/Control_structures
http://julio.meroh.net/2006/08/split-function-in-haskell.html
https://stackoverflow.com/questions/19275100/string-split-into-6-parts-every-character-in-haskell
https://codereview.stackexchange.com/questions/6992/approach-to-string-split-by-character-in-haskell
https://github.com/haskell/filepath/blob/master/Generate.hs
https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Maybe.html
https://github.com/sunahkim15/449-haskell/blob/master/FileIO.hs
https://stackoverflow.com/questions/20156078/replacing-an-element-in-a-list-of-lists-in-haskell
-}
module Parsing where
import System.IO
import System.FilePath
import System.Environment
import System.Exit
import Data.Char
import Data.List
import Data.Bool
import Data.Maybe

--Parsing Function
--Simply divides the string into substrings starting from the bottom of the file
parsing :: [String] -> [[String]]
parsing pX = cpX
    where iTNP = fromJust $ head $ indexOf pX "too-near penalities"
          sTNP = head (tail $ pair2List $ head $ indexedList pX iTNP) 
          gotTNP = head $ pair2List $ head $ indexedList pX iTNP
          iMP = fromJust $ head $ indexOf gotTNP  "machine penalties:"
          sMP = head (tail $ pair2List $ head $ indexedList gotTNP iMP)
          gotMP = head $ pair2List $ head $ indexedList gotTNP iMP
          iTNT = fromJust $ head $ indexOf gotMP "too-near tasks:"
          sTNT = head (tail $ pair2List $ head $ indexedList gotMP iTNT)
          gotTNT = head $ pair2List $ head $ indexedList gotMP iTNT
          iFM = fromJust $ head $ indexOf gotTNT "forbidden machine:"
          sFM = head (tail $ pair2List $ head $ indexedList gotTNT iFM)
          gotFM = head $ pair2List $ head $ indexedList gotTNT iFM
          iFPA = fromJust $ head $ indexOf gotFM "forced partial assignment:"
          sFPA = head (tail $ pair2List $ head $ indexedList gotFM iFPA)
          gotFPA = head $ pair2List $ head $ indexedList gotFM iFPA
          iName = fromJust $ head $ indexOf gotFPA "name:"
          sName = head (indexedList gotFPA iName)
          cpX' = sFPA : sFM : sTNT : sMP : sTNP : []
          cpX = map tail cpX'
{-Parsing Function
-- Simply divides the string into substrings starting from the top of the file
parsing :: [String] -> [[String]]
parsing pX = cpX
	where setFile = map lastChar (lines (checkEmpty (map toUpper contents)))  -- Converting all strings to upper charecters for checking
        --Get sections of strings from the file to check for parsing
        partName   = partString "NAME:" "FORCED PARTIAL ASSIGNMENT:" setFile
        partForced = partString "FORCED PARTIAL ASSIGNMENT:" "FORBIDDEN MACHINE:" setFile
        partForbid = partString "FORBIDDEN MACHINE:" "TOO-NEAR TASKS:" setFile
        part2near  = partString "TOO-NEAR TASKS:" "MACHINE PENALTIES:" setFile
        partMpenal = partString "MACHINE PENALTIES:" "TOO-NEAR PENALTIES:" setFile
        part2penal = partString "TOO-NEAR PENALTIES:" "" setFile
		cpX' = partName : partForced : partForbid : part2near : partMpenal : part2penal : []
        cpX = map tail cpX'
-}
--Trimming Strings
trim :: String -> String
trim = x . x
   where x = reverse . dropWhile isSpace
   

--Return String without whitespce
noEmpty :: [String] -> [String]
noEmpty pX = pX''
    where pX' = filter (/=" ") pX
          pX'' = filter (/="") pX'

--Return a tuple of 2 List
pair2List :: (a, a) -> [a]
pair2List (x, y) = [x, y]

--Return a tuple of 3 List
pair3List :: (a, a, a) -> [a]
pair3List (x, y, z) = [x, y, z]

--Return index within String Array
indexOf :: [String] -> String-> [Maybe Int]
indexOf pX str = 
    return (elemIndex str pX)


-- returns the left hand side of the string
takeTo :: String -> [String] -> [String]
takeTo _ [] = []
takeTo [] x = x
takeTo y (x:xs)    
    | y /= x = x:(takeTo y xs)
    | otherwise = []

-- returns the right hand side of the string
takeFrom :: String -> [String] -> [String]
takeFrom _ [] = []
takeFrom [] x = x
takeFrom y (x:xs) 
    | y /= x    = takeFrom y xs
    | otherwise = xs

--Part the string into different givin sections
partString :: String -> String -> [String] -> [String]
partString _ _ [] = []
partString [] y z = takeTo y z
partString x [] z = takeFrom x z
partString x y z  = takeFrom x (takeTo y z)

--Divide List from Index
indexedList :: [String] -> Int -> [([String], [String])]
indexedList pX ind = do
    let parsedList = splitAt ind pX
    return parsedList


--Return Values from within tuples: (_,_)
inTuples :: String -> String
inTuples pX = newpX
    where pX' = delete ')' pX
          pX'' = delete '(' pX'
          newpX = filter (/=',') pX''

--Str to Integer function
strIntVal :: [String] -> [Int]
strIntVal pX = pX' 
    where pX' = [read x :: Int | x <- pX]

--Return a 2D array from an Array with line contents	
toArray :: [String] -> [[String]]
toArray pX = gotTuple
    where pX' = noEmpty pX
          pX'' = map inTuples pX'
          gotTuple = map pair2List gotTuple'
          gotTuple' = zip one two
          one = [toString x | x <- one']
          one' = [x !! 0 | x <- pX'']
          two = [toString x | x <- two']
          two' = [x !! 1 | x <- pX'']
toArray' :: [String] -> [[String]]
toArray' pX = gotTuple 
    where pX' = noEmpty pX  
          pX'' = map inTuples pX'
          gotTuple = map pair3List gotTuple'
          gotTuple' = zip3 one two third
          one = [toString x | x <- one']
          one' = [x !! 0 | x <- pX'']
          two = [toString x | x <- two']
          two' = [x !! 1 | x <- pX'']
          third = [toString x | x <- third']
          third' = [x !! 2 | x <- pX'']

--Return string value of char: 'a' -> "a"
toString :: Char -> String
toString chr = [chr]

--Return numeric task value of Task
whichTask :: String -> Int
whichTask str 
    | str == "A" = 1
    | str == "B" = 2
    | str == "C" = 3
    | str == "D" = 4
    | str == "E" = 5
    | str == "F" = 6
    | str == "G" = 7
    | otherwise = -1
	
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

--Return Numeric Tuple from string
tupleVal :: [[a]] -> a -> (Int, Int) -> [[a]]
tupleVal pX val (x, y) = take x pX ++ [take y (pX !! x) ++ [val]] ++ [drop (y+1) (pX !! x)] ++ drop (x+1) pX


--Return Numeric task task tuple value
tupleTask :: [String] -> (Int, Int)
tupleTask [x, y] = (x', y')
    where x' = whichTask x - 1
          y' = whichTask y - 1

--Return Numeric mach task tuple value
tupleMT :: [String] -> (Int, Int)
tupleMT [x, y] = (x', y')
    where x' = (read x :: Int) - 1
          y' = whichTask y

--Return machine penalty Int Value
mpVal :: [String] -> [[Int]]
mpVal pX = mp 
    where pX' = [words x | x <- pX]
          mp = [strIntVal x | x <- pX']         
mpVal' :: [String] -> [[String]]
mpVal' pX = mp
    where mp = [words x | x <- pX]


{-
USING CHECK METHODS FOR BOOLEAN COMPUTATIONS
-}
--Returns false if machine penalties do not follow parsing constraints
checkMP :: [[String]] -> Bool
checkMP pX = x && y
    where x = length pX == 8
          y = checkLength pX 7
--Return number of penalties per line for former boolean checking
checkLength :: [[String]] -> Int -> Bool
checkLength pX n
     | n < 0     = True
     | otherwise = (checkLength pX (n-1)) && (length pX' == 8) 
    where pX' = pX !! n

--Return false if given TNT is invalid
checkTNT :: [[String]] -> Bool
checkTNT pX = val
    where val = checkTNT' pX' n
          pX' = toArray (pX !! 4)
          n = length pX' - 1
checkTNT' :: [[String]] -> Int -> Bool
checkTNT' [] n = True
checkTNT' sTasks n 
     | n < 0     = True
     | otherwise = (checkTNT' sTasks (n-1)) && x
    where x = y && z
          y = (pX !! 0) `elem` tasks
          z = (pX !! 1) `elem` tasks
          pX = sTasks !! n
          tasks = ["A", "B", "C", "D", "E", "F", "G", "H"]

--Return false if value withing tuple is inccorect
checkPval :: [[String]] -> Bool
checkPval pX = val
    where val = correctMach && correctTask
          correctMach = checkPval' mp 7
          correctTask = checkPval'' sp sLength
          mp = mpVal' (pX !! 3) 
          sp = toArray' (pX !! 4)  
          sLength = length sp - 1 
checkPval' :: [[String]] -> Int -> Bool
checkPval' pX n 
     | n < 0     = True
     | otherwise = (checkPval' pX (n-1)) && (checkNatural pX' 7)
    where pX' = pX !! n
checkPval'' :: [[String]] -> Int -> Bool
checkPval'' [] n = True
checkPval'' sTasks n 
     | n < 0     = True
     | otherwise = (checkPval'' sTasks (n-1)) && x
    where x = y > (-1)
          y = checkString z
          z = pX !! 2
          pX = sTasks !! n
          

--Return true if Number is a natural number >= 0
checkNatural :: [String] -> Int -> Bool
checkNatural pX n
    | n < 0 = True
    | otherwise = (checkNatural pX (n-1)) && x
    where x = checkNatural' y
          y = pX !! n		  
checkNatural' :: String -> Bool
checkNatural' penalty
    | ('.' `elem` penalty) = False	-- Number is a double/float
    | otherwise = penalty' > 0  --Number is less than 0
	where penalty' = read penalty :: Int

--Return True if element is an alphabet
checkString :: String -> Int
checkString [a]
    | (a `notElem` ['A'..'Z']) == True = read [a] :: Int
    | otherwise = -1

{-Fuctions for booleaning checking in main (Not Working)
checkBool2pair :: [String] -> Bool
checkBool2pair [] = False
checkBool2pair x
    | checkBool (map check2Pair(removeEmpty x)) == True = True
    | otherwise = False
checkBool3pair :: [String] -> Bool
checkBool3pair [] = False
checkBool3pair x
    | checkBool (map check3Pair(removeEmpty x)) == True = True
    | otherwise = False
-}

-- checks the number of commas in a given string
checkComma :: String -> Int
checkComma []  = 0
checkComma (x:xs)  
    | (x == ',')= 1 + (checkComma xs)
    | otherwise = checkComma xs
	
--Return true if mach task tuple is valid
checkMT :: [[String]] -> Bool  
checkMT pX = mBool && tBool 
    where mBool = checkMach pX
          tBool = checkTask pX
--Return true if M value is valid
checkMach :: [[String]] -> Bool
checkMach pX = correctFPA && correctFM
    where correctFPA = checkMach' sFPA newLength
          correctFM = checkMach' sFM newLength'
          sFPA = toArray (pX !! 0) 
          sFM = toArray (pX !! 1)
          newLength = length sFPA - 1
          newLength' = length sFM - 1
checkMach' :: [[String]] -> Int -> Bool
checkMach' [] n = True
checkMach' pX n 
     | n < 0     = True
     | otherwise = (checkMach' pX (n-1)) && x
    where x = y `elem` machines
          y = pX' !! 0
          pX' = pX !! n
          machines = ["1", "2", "3", "4", "5", "6", "7", "8"]
--Return true if T value is valid
checkTask :: [[String]] -> Bool
checkTask pX = correctFPA && correctFM && correctTNT
    where correctFPA = checkTask' sFPA newLength
          correctFM = checkTask' sFM newLength'
          sFPA = toArray (pX !! 0) 
          newLength = length sFPA - 1
          sFM = toArray (pX !! 1)
          newLength' = length sFM - 1
          correctTNT = checkTask'' sTNT lenTNT
          sTNT = toArray (pX !! 2) 
          lenTNT = length sTNT - 1
checkTask' :: [[String]] -> Int -> Bool
checkTask' [] n = True
checkTask' pX n
     | n < 0     = True
     | otherwise = (checkTask' pX (n-1)) && x
    where x = y `elem` tasks
          y = pX' !! 1
          pX' = pX !! n
          tasks = ["A", "B", "C", "D", "E", "F", "G", "H"]
checkTask'' :: [[String]] -> Int -> Bool
checkTask'' [] n = True
checkTask'' pX n
     | n < 0     = True
     | otherwise = (checkTask'' pX (n-1)) && x
    where x = y && z
          y = y' `elem` tasks
          z = z' `elem` tasks
          y' = pX' !! 0
          z' = pX' !! 1
          pX' = pX !! n
          tasks = ["A", "B", "C", "D", "E", "F", "G", "H"]

-- Check if there exists a partial assignment that is invalid
checkFPA :: [[String]] -> Bool
checkFPA pX = checkFPA' pX && checkFPA'' pX
checkFPA' :: [[String]] -> Bool
checkFPA' pX = tNew && mNew
    where sFPA = toArray (pX !! 0) 
          tFPA = [last x | x <- sFPA]
          mFPA = [head x | x <- sFPA]
          tNew = newAssignments tFPA ft
          mNew = newAssignments mFPA fm
          ft = length tFPA - 1
          fm = length mFPA - 1
checkFPA'' :: [[String]] -> Bool
checkFPA'' pX = checkAssignments sFPA sFM n
    where sFPA = toArray (pX !! 0)
          sFM = toArray (pX !! 1)
          n = length sFPA - 1

-- Check if all the inputs are valid and returns false if they aren't
isCorrect :: [[String]] -> Bool
isCorrect pX
     | boolFPA == False = False
     | boolFM  == False = False
     | boolMP  == False = False
     | boolT   == False = False
     | boolP   == False = False
     | otherwise = True
    where boolFPA = checkFPA pX
          boolFM  = checkMT pX
          mp      = mpVal' (pX !! 3)
          boolMP  = checkMP mp
          boolT   = checkTNT pX
          boolP   = checkPval pX
		  
--Adding reference to a new assingment
newAssignments :: [String] -> Int -> Bool
newAssignments pX n
     | n < 0    = True
     | otherwise = (newAssignments pX (n-1)) && (x `notElem` pX')
    where pX' = take n pX
          x = pX !! n
--Checking if newly assignmened task already exists
checkAssignments :: [[String]] -> [[String]] -> Int -> Bool
checkAssignments [] _ _ = True
checkAssignments _ [] _ = True
checkAssignments sFPA sFM n
     | n < 0     = True
     | otherwise = (x `notElem` sFM) && checkAssignments sFPA sFM (n-1)
    where x = sFPA !! n

-- Get error message to print to output file
-- If there is no error, message will be "No error"
error' :: [[String]] -> String
error' pX 
     | isValid == False = error'' pX
     | otherwise        = "File parsed successfully" 
    where isValid = isCorrect pX
error'' :: [[String]] -> String
error'' pX
     | boolFPA == False = "partial assignment error"
     | boolFM  == False = "invalid machine/task"
     | boolMP  == False = "machine penalty error"
     | boolT   == False = "invalid task"
     | boolP   == False = "invalid penalty"
	 | otherwise = "Error while parsing input file"
    where boolFPA = checkFPA pX
          boolFM  = checkMT pX
          mp      = mpVal' (pX !! 3)
          boolMP  = checkMP mp
          boolT   = checkTNT pX
          boolP   = checkPval pX
   
--Main Function
main = do

  --NEW PARSER WITHOUT IF-ELSE STATEMENTS, ONLY MONADS FOR ERROR Checking
  
  args <- getArgs
  handle <- openFile (head args) ReadMode
  --outHandle <- openFile (last args) WriteMode
  
  contents <- hGetContents handle
  
  let gotContents = lines contents
  let afterParsing = map noEmpty (parsing gotContents)
  let outputStr = error' afterParsing
  {-Fuction not working correctly (Not Working)
    -- to check if the total given machine penalties is 8x8
    if(length partMpenal == 64)
    then return()
    else do writeFile (last args) "machine penalty error"
            exitSuccess
  -}
  
  outHandle <- openFile (last args) WriteMode
  hPutStrLn outHandle (outputStr)
  
  hClose outHandle
  hClose handle