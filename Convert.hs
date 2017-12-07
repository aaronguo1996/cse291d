import Control.Monad
import Data.List
import Data.Strings
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Extension
import System.Environment
import System.IO
import System.Random
import System.Process
import System.Exit
import Enumeration
import StreamFusion
import Helper

-- | a helper function to split the read file into separate lines
-- The only argument for this function is of type 'String' which denotes 
-- the input fileline
getLines :: String -> [String]
getLines file = lines file

-- | hi
getHoleByLine :: String -> Int
getHoleByLine []     = 0
getHoleByLine (x:xs)
    | x == '?'  = 1 + getHoleByLine xs 
    | otherwise = getHoleByLine xs

-- | function
getHoles :: [String] -> Int
getHoles fileline = Prelude.foldl (\a l -> a + (getHoleByLine l)) 0 fileline

-- | a helper function for getHoleIdx to generate a sequence of integers
-- The first argument is an integer which is the sequence top
sequenceTo :: Int -> [[Int]]
sequenceTo top
    | top > 0   = (sequenceTo (top-1)) ++ [[top]]
    | otherwise = [[0]]

-- | a helper function to add a sequence of up to element to each of the given 
-- lists and get a new list of lists with their multiplication
-- The first argument is the added element
-- The second argument is the given lists
appendTo :: Int -> [[Int]] -> [[Int]]
appendTo (-1) tails = []
appendTo elmt tails = (appendTo (elmt - 1) tails) ++ (Prelude.map ((:) elmt) tails)

-- | the function 'getHoleIdx' assign holes by the number of holes and 
-- number of the candidates
-- The first argument is an Int denotes the number of holes
-- The second argument is an Int denotes the number of candidates
getHoleIdx :: Int -> Int -> [[Int]]
getHoleIdx holes cands
    | holes == 1 = sequenceTo cands
    | otherwise  = appendTo cands (getHoleIdx (holes - 1) cands)

-- Helper function which returns the nth element in a list
nth :: Int -> [a] -> a
nth 0 (x : _ ) = x
nth n (_ : xs) = nth (n - 1) xs

-- | the function 'fillHoles' replace the holes in the program by specific code
-- fragments according to their assigned indices
fillHoles :: [Int] -> Int -> String -> String
fillHoles frags idx []     = []
fillHoles frags idx (x:xs)
    | x /= '?'     = x : (fillHoles frags idx xs)
    | otherwise    = thisHole ++ remainingHoles
  where
    thisHole       = exprToStr (expand (nth idx frags) ["s1"])
    remainingHoles = fillHoles frags (idx+1) xs
-- |
convertQM :: [Int] -> Int -> [String] -> [String]
convertQM frags idx []     = []
convertQM frags idx (x:xs) = 
    hline : tlines
  where
    hline  = fillHoles frags idx x
    tlines = convertQM frags (idx + (getHoleByLine x)) xs

-- |
randomList :: Int -> (Int,Int) -> IO([Int])
randomList 0 _    = return []
randomList n bnds = do
    r  <- randomRIO bnds
    rs <- randomList (n-1) bnds
    return (r:rs) 
--randomList :: (Random a) => (a,a) -> Int -> StdGen -> [a]
--randomList bnds n = take n . randomRs bnds

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

-- | 
testFilter :: String -> IO Bool
testFilter codeFile = do
    let p = ((\x -> x > 10) :: Int -> Bool)
    testCase <- (randomList 10 (1,100)) -- Some magic numbers here
    (exitCode,testRes,errMsg) <- readProcessWithExitCode "runhaskell" ["-XExistentialQuantification",codeFile,(listToStr testCase)] ""
    case exitCode of
        ExitSuccess -> do
                        let expcRes = show (filter p testCase)
                        let cleanRes = deleteAt ((Data.List.length testRes) - 1) testRes
                        return (cleanRes == expcRes)
        ExitFailure i -> do 
                        putStrLn $ show errMsg
                        return False
    {-r <- runInterpreter $ do
            set [languageExtensions := [asExtension "ExistentialQuantification"]]
            setImports ["Prelude"]
            loadModules [codeFile,"StreamFusion.hs"]
            interpret ("\\p l-> filters p l") (as :: (Int -> Bool) -> Stream Int -> Stream Int)
    case r of
        Left err -> do
                    putStrLn (errorString err)
                    return False
        Right f  -> return True--((unstream (f p (stream testCase))) == (filter p testCase))
-}
-- TODO: update the convertQM function call and add getHoleIdx to the method
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
                    putStrLn "Wrong program, new iteration"
                    (iteration fraglist (idx+1) filelines)

main = do
    [f] <- getArgs
    file <- readFile f
    let filelines = getLines file
    reslines <- iteration (getHoleIdx (getHoles filelines) 3) 0 filelines
    putStrLn $ unlines reslines