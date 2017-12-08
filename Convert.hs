import Control.Monad
import Data.List
import Data.Strings
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Extension
import System.Environment
import System.IO
import System.Process
import System.Exit
import Enumeration
import StreamFusion
import Helper

-- | A helper function to split the read file into separate lines
-- The only argument for this function is of type 'String' which denotes 
-- the input fileline
getLines :: String -> [String]
getLines file = lines file

-- | A helper function to count the number of holes in a given line
-- The only argument for this function is of type 'String' which is the 
-- line with holes in it
getHoleByLine :: String -> Int
getHoleByLine []     = 0
getHoleByLine (x:xs)
    | x == '?'  = 1 + getHoleByLine xs 
    | otherwise = getHoleByLine xs

-- | A helper function to count all the number of holes in a code segment
-- The first argument is of type '[String]'
-- which should be the code segment we need to process 
getHoles :: [String] -> Int
getHoles fileline = Prelude.foldl (\a l -> a + (getHoleByLine l)) 0 fileline

-- | The function 'getHoleIdx' assign holes by the number of holes and 
-- number of the candidates
-- The first argument is an Int denotes the number of holes
-- The second argument is an Int denotes the number of candidates
getHoleIdx :: Int -> Int -> [[Int]]
getHoleIdx holes cands
    | holes == 1 = sequenceTo cands
    | otherwise  = appendTo cands (getHoleIdx (holes - 1) cands)

-- | The function 'fillHoles' replace the holes in the program by specific code
-- fragments according to their assigned indices
fillHoles :: [Int] -> Int -> String -> String
fillHoles frags idx []     = []
fillHoles frags idx (x:xs)
    | x /= '?'     = x : (fillHoles frags idx xs)
    | otherwise    = thisHole ++ remainingHoles
  where
    thisHole       = exprToStr (expand (nth idx frags) ["s1"])
    remainingHoles = fillHoles frags (idx+1) xs

-- | The function 'convertQM' replace the question marks in code segment by
-- calling the function 'fillHoles' and concatenate the results
-- The first argument is of type '[Int]' which means the code segment assigned 
-- to each hole in the given code consequently
-- The second argument is of type 'Int' which means the index of current hole
-- The third argument is of type '[String]' which is the code to be processed 
convertQM :: [Int] -> Int -> [String] -> [String]
convertQM frags idx []     = []
convertQM frags idx (x:xs) = 
    hline : tlines
  where
    hline  = fillHoles frags idx x
    tlines = convertQM frags (idx + (getHoleByLine x)) xs

-- | The function 'testFilter' is for filters testing
-- The first argument is of type 'String' which is the code to be executed
-- In this function, we call the 'runhaskell' in the shell command and get the
-- output to compare whether they have the same behavior as the built-in filter
testFilter :: String -> IO Bool
testFilter codeFile = do
    let p = ((\x -> x > 75) :: Int -> Bool)
    testCase <- (randomList 10 (1,100)) -- Some magic numbers here
    (exitCode,testRes,errMsg) <- readProcessWithExitCode "runhaskell" ["-XExistentialQuantification",codeFile,(listToStr testCase)] ""
    case exitCode of
        ExitSuccess -> do
                        putStrLn $ show (map (\x->if x<50 then x+50 else x) testCase)
                        let expcRes = show (filter p (map (\x->if x<50 then x+50 else x) testCase))
                        let cleanRes = deleteAt ((Data.List.length testRes) - 1) testRes
                        putStrLn $ expcRes
                        putStrLn $ cleanRes
                        return (cleanRes == expcRes)
        ExitFailure i -> do 
                        putStrLn $ errMsg
                        readProcessWithExitCode "rm" [codeFile] ""
                        return False

-- | The function 'iteration' iteratively search in the components to find a
-- program with proper behavior we expected
-- The first argument is of type '[[Int]]' which is all the possible assignment
-- of indices for the holes in the code segment
-- The second argument is of type 'Int' which is index of current assignment
-- The third argument is of type '[String]' which is the code to be processed
-- In this function we add some decorated code to the original code sketch so 
-- that the following steps have certain properties we want.
iteration :: [[Int]] -> Int -> [String] -> IO [String]
iteration fraglist idx filelines = do
    if (idx > Data.List.length fraglist)
        then return ["Failed"]
        else do
            let code = convertQM (nth idx fraglist) 0 filelines
            let codeFile = "Filters" ++ show idx ++ ".hs"
            let decCode = ["module Filters" ++ show idx ++"(filters) where",""] ++ code
            writeFile codeFile $ unlines decCode
            success <- testFilter codeFile
            if success
                then do 
                    putStrLn $ unlines decCode
                    return decCode
                else do
                    putStrLn "Wrong program, new iteration..."
                    readProcessWithExitCode "rm" [codeFile] ""
                    (iteration fraglist (idx+1) filelines)

-- | This is the main function to be executed
main = do
    [f] <- getArgs
    file <- readFile f
    let filelines = getLines file
    reslines <- iteration (getHoleIdx (getHoles filelines) 3) 0 filelines
    putStrLn $ unlines reslines