import System.IO
import System.Random
import Data.List

---- let someString = map (\x -> (show (fst x)) ++ "," ++ (show (snd x))) rp

main :: IO ()
main = do 
          
          
          outputMultipleFiles 200

          
          

          putStrLn "Done"
atRandIndex :: [a] -> IO a  -- note that this is gives itself an IO action
atRandIndex l = do
    i <- randomRIO (0, length l - 1)
    return $ l !! i

randomPairs  letter = map (\x -> (x,letter)) [1..8]

charList = ['A','B','C','D','E','F','G','H']

numberList = [1..99]

--output File for invalidForbdden (all machine having the same tasks)
outputFile :: String -> IO ()
outputFile fileName = do
          

          

          randomLetter  <- atRandIndex charList

          

          let rp = randomPairs randomLetter

          let someString = map (\x -> "(" ++ (show (fst x)) ++ "," ++ ([(snd x)]) ++ ")") rp


          let oneLineRpStr = foldl1 (\x y -> x ++ "\n" ++ y) someString

          let outputString = "Name:\n" ++ "invalidforbidden\n" ++ "\n" ++ "forced partial assignment:\n" ++ "\n" ++
               "forbidden machine:\n" ++ oneLineRpStr ++ "\n" ++ "\n" ++ "too-near tasks:\n" ++ "\n" ++ "machine penalties:\n" ++ "1 1 1 1 1 1 1 1" ++ "\n"
               ++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"
               ++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++
               "\n" ++ "too-near penalities" 

          writeFile fileName outputString

          putStrLn ("Done with output file " ++ fileName)


--wrongpartialassignment (1 machine maybe assignment with more than one task)
outputFileTwo :: String -> IO ()
outputFileTwo fileName = do
          

          

          letter1  <- atRandIndex charList
          letter2  <- atRandIndex charList
          letter3  <- atRandIndex charList
          letter4  <- atRandIndex charList
          letter5  <- atRandIndex charList
          letter6  <- atRandIndex charList
          letter7  <- atRandIndex charList
          letter8  <- atRandIndex charList

          let rp = [ (1,letter1)  , (2,letter2), (3,letter3), (4,letter4), (5,letter5), (6,letter6), (7,letter7), (8,letter8) ]


     
          let someString = map (\x -> "(" ++ (show (fst x)) ++ "," ++ ([(snd x)]) ++ ")") rp


          let oneLineRpStr = foldl1 (\x y -> x ++ "\n" ++ y) someString

          let outputString = "Name:\n" ++ "wrongpartialassignment\n" ++ "\n" ++ "forced partial assignment:\n" ++ oneLineRpStr ++ "\n" ++ "\n" ++
               "forbidden machine:\n" ++ "\n" ++ "too-near tasks:\n" ++ "\n" ++ "machine penalties:\n" ++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"
                ++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n" ++
               "\n" ++ "too-near penalities"

          writeFile fileName outputString

          putStrLn ("Done with output file " ++ fileName)
 
-- making output file about each machine gets a different task it might compute correct input file or it may output file that with forced partial machine
outputFileThree :: String -> IO ()
outputFileThree fileName = do
          

          let cList1 = charList

          letter1  <- atRandIndex cList1
          let cList2 = delete letter1 cList1

          letter2  <- atRandIndex cList2
          let cList3 = delete letter2 cList2

          letter3  <- atRandIndex cList3
          let cList4 = delete letter3 cList3

          letter4  <- atRandIndex cList4
          let cList5 = delete letter4 cList4

          letter5  <- atRandIndex cList5
          let cList6 = delete letter5 cList5

          letter6  <- atRandIndex cList6
          let cList7 = delete letter6 cList6

          letter7  <- atRandIndex cList7
          let cList8 = delete letter7 cList7

          let letter8  = cList8 !! 0

          let rp = [ (1,letter1)  , (2,letter2), (3,letter3), (4,letter4), (5,letter5), (6,letter6), (7,letter7), (8,letter8) ]


     
          let someString = map (\x -> "(" ++ (show (fst x)) ++ "," ++ ([(snd x)]) ++ ")") rp


          let oneLineRpStr = foldl1 (\x y -> x ++ "\n" ++ y) someString

          let outputString = "Name:\n" ++ "invalidforbidden\n" ++ "\n" ++ "forced partial assignment:\n" ++ oneLineRpStr ++ "\n" ++
               "forbidden machine:\n" ++ "\n" ++ "too-near tasks:\n" ++ "\n" ++ "machine penalties:\n" ++  "1 1 1 1 1 1 1 1" ++ "\n" ++  "1 1 1 1 1 1 1 1" ++ "\n"
               ++  "1 1 1 1 1 1 1 1" ++ "\n"++  "1 1 1 1 1 1 1 1" ++ "\n"++  "1 1 1 1 1 1 1 1" ++ "\n"++  "1 1 1 1 1 1 1 1" ++ "\n"++  "1 1 1 1 1 1 1 1" ++ "\n"
               ++  "1 1 1 1 1 1 1 1" ++ "\n" ++ "\n" ++ "too-near penalities"
          writeFile fileName outputString

          putStrLn ("Done with output file " ++ fileName)

--output file that has no option
outputFileFour :: String -> IO ()
outputFileFour fileName = do

          let outputString = "Name:\n" ++ "noOption\n" ++ "\n" ++ "forced partial assignment:\n" ++ "\n" ++
               "forbidden machine:\n" ++ "\n" ++ "too-near tasks:\n" ++ "\n" ++ "machine penalties:\n" ++ "1 1 1 1 1 1 1 1" ++ "\n" ++ "1 1 1 1 1 1 1 1" ++ "\n" ++ "1 1 1 1 1 1 1 1" ++ "\n"
               ++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "\n" ++ "too-near penalities" 

          writeFile fileName outputString

          putStrLn ("Done with output file " ++ fileName)

--output typo files
outputFileFive :: String -> IO ()
outputFileFive fileName = do

          let cList1 = charList

          letter1  <- atRandIndex cList1
          let cList2 = delete letter1 cList1

          letter2  <- atRandIndex cList2
          let cList3 = delete letter2 cList2

          letter3  <- atRandIndex cList3
          let cList4 = delete letter3 cList3

          letter4  <- atRandIndex cList4
          let cList5 = delete letter4 cList4

          letter5  <- atRandIndex cList5
          let cList6 = delete letter5 cList5

          letter6  <- atRandIndex cList6
          let cList7 = delete letter6 cList6

          letter7  <- atRandIndex cList7
          let cList8 = delete letter7 cList7

          let letter8  = cList8 !! 0

          let rp = [ (1,letter1)  , (2,letter2), (3,letter3), (4,letter4), (5,letter5), (6,letter6), (7,letter7), (8,letter8) ]


     
          let someString = map (\x -> "(" ++ (show (fst x)) ++ "," ++ ([(snd x)]) ++ ")") rp


          let oneLineRpStr = foldl1 (\x y -> x ++ "\n" ++ y) someString
          let outputString = "Name:\n" ++ "typo\n" ++ "\n" ++ "force partial assignment:\n" ++ oneLineRpStr ++ "\n" ++
               "forbidden machine:\n" ++ "\n" ++ "too-near tsaks:\n" ++ "\n" ++ "machine penalties:\n" ++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"
               ++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++ "1 1 1 1 1 1 1 1" ++ "\n"++
               "\n" ++ "too-near penalities:"

          writeFile fileName outputString

          putStrLn ("Done with output file " ++ fileName)


--output file that without penalities
outputFileSix :: String -> IO ()
outputFileSix fileName = do
          

          let cList1 = charList

          letter1  <- atRandIndex cList1
          let cList2 = delete letter1 cList1

          letter2  <- atRandIndex cList2
          let cList3 = delete letter2 cList2

          letter3  <- atRandIndex cList3
          let cList4 = delete letter3 cList3

          letter4  <- atRandIndex cList4
          let cList5 = delete letter4 cList4

          letter5  <- atRandIndex cList5
          let cList6 = delete letter5 cList5

          letter6  <- atRandIndex cList6
          let cList7 = delete letter6 cList6

          letter7  <- atRandIndex cList7
          let cList8 = delete letter7 cList7

          let letter8  = cList8 !! 0

          let rp = [ (1,letter1)  , (2,letter2), (3,letter3), (4,letter4), (5,letter5), (6,letter6), (7,letter7), (8,letter8) ]


     
          let someString = map (\x -> "(" ++ (show (fst x)) ++ "," ++ ([(snd x)]) ++ ")") rp


          let oneLineRpStr = foldl1 (\x y -> x ++ "\n" ++ y) someString

          let outputString = "Name:\n" ++ "withoutpenalities\n" ++ "\n" ++ "forc partial assignment:\n" ++ oneLineRpStr ++ "\n" ++
               "forbidden machine:\n" ++ "\n" ++ "too-near tsaks\n" ++ "\n" ++ "machine penalties:\n" ++ "\n" ++ "too-near penalities:" 

          writeFile fileName outputString

          putStrLn ("Done with output file " ++ fileName)


outputMultipleFiles :: Integer -> IO()
outputMultipleFiles number 
     | number == 0 = do putStrLn ("Done with outputMultipleFiles  ")
     | otherwise = do
                    outputFile ("invalidForbdden" ++ (show number) ++ ".txt" )
                    outputFileTwo ("wrongpartialassignment" ++ (show number) ++ ".txt")
                    outputFileThree ("forcedPartial" ++ (show number) ++ ".txt")
                    outputFileFour ("noOption" ++ (show number) ++ ".txt")
                    outputFileFive ("typo" ++ (show number) ++ ".txt")
                    outputFileSix ("withoutpenalities" ++ (show number) ++ ".txt")
                    outputMultipleFiles (number - (1))


