{-# LANGUAGE ExistentialQuantification #-}

import Control.Monad
import Data.List
import Data.Strings
import Data.Dynamic
--import Language.Haskell.Interpreter
--import Language.Haskell.Interpreter.Extension
import System.Environment
import System.IO
import System.Process
import System.Exit
import Enumeration
--import StreamFusion
import Helper
import Types
import Parser

-- | A helper function to split the read file into separate lines
-- The only argument for this function is of type 'String' which denotes 
-- the input fileline
getLines :: String -> [String]
getLines file = lines file

-- | A helper function to count the number of holes in a given line
-- The only argument for this function is of type 'String' which is the 
-- line with holes in it
-- getHoleByLine :: String -> Int
-- getHoleByLine []     = 0
-- getHoleByLine (x:xs)
--     | x == '?'  = 1 + getHoleByLine xs 
--     | otherwise = getHoleByLine xs

-- | A helper function to count all the number of holes in a code segment
-- The first argument is of type '[String]'
-- which should be the code segment we need to process 
-- getHoles :: [String] -> Int
-- getHoles fileline = Prelude.foldl (\a l -> a + (getHoleByLine l)) 0 fileline

-- | The function 'getHoleIdx' assign holes by the number of holes and 
-- number of the candidates
-- The first argument is an Int denotes the number of holes
-- The second argument is an Int denotes the number of candidates
-- getHoleIdx :: Int -> Int -> [[Int]]
-- getHoleIdx holes cands
--     | holes == 1 = sequenceTo cands
--     | otherwise  = appendTo cands (getHoleIdx (holes - 1) cands)
getHoleNum :: Expr -> Int
getHoleNum expr = case expr of
    EDone -> 0
    ESkip e -> getHoleNum e
    EYield e1 e2 -> (getHoleNum e1) + (getHoleNum e2)
    Var v -> if (strHasHole v) then 1 else 0
    App e1 e2 -> (getHoleNum e1) + (getHoleNum e2)
    Tuple e1 e2 -> (getHoleNum e1) + (getHoleNum e2)
    Enumeration.Fun name params e -> (foldl (+) 0 (map getHoleNum params)) + (getHoleNum e)
    Stream s1 s2 -> (if (strHasHole s1) then 1 else 0) + (if (strHasHole s2) then 1 else 0)
    Where e es -> (getHoleNum e) + (foldl (+) 0 (map getHoleNum es))
    Switch e es -> (getHoleNum e) + (foldl (+) 0 (map getHoleNum (fst (unzip es)))) + (foldl (+) 0 (map getHoleNum (snd (unzip es))))
-- | The function 'fillHoles' replace the holes in the program by specific code
-- fragments according to their assigned indices
-- fillHoles :: [Int] -> Int -> String -> [(Expr,TypeExpr)] -> String
-- fillHoles frags idx [] _    = []
-- fillHoles frags idx (x:xs) comps
--     | x /= '?'     = x : (fillHoles frags idx xs comps)
--     | otherwise    = thisHole ++ remainingHoles
--   where
--     thisHole       = exprToStr (expand (nth idx frags) comps)
--     remainingHoles = fillHoles frags (idx+1) xs comps

fillParamHoles :: [Int] -> Int -> [(Expr,TypeExpr)] -> [Expr] -> [Expr]
fillParamHoles frags idx comps [] = []
fillParamHoles frags idx comps (x:xs) = 
    (fillHoles frags idx comps x):(fillParamHoles frags (idx+getHoleNum x) comps xs)

fillSwitchHoles :: [Int]->Int->[(Expr,TypeExpr)]->[(Expr,Expr)]->[(Expr,Expr)]
fillSwitchHoles frags idx comps [] = []
fillSwitchHoles frags idx comps (x:xs) =
    (fillHoles frags idx comps (fst x),fillHoles frags (idx+getHoleNum (fst x)) comps (snd x)):(fillSwitchHoles frags (idx+(getHoleNum (fst x))+(getHoleNum (snd x))) comps xs)

fillHoles :: [Int] -> Int -> [(Expr,TypeExpr)] -> Expr -> Expr
fillHoles frags idx comps expr = case expr of
    EDone -> EDone
    ESkip e -> ESkip (fillHoles frags idx comps e)
    EYield e1 e2 -> EYield (fillHoles frags idx comps e1) (fillHoles frags (idx+(getHoleNum e1)) comps e2)
    Var v -> if (strHasHole v) then fst (nth (nth idx frags) comps) else expr
    App e1 e2 -> App (fillHoles frags idx comps e1) (fillHoles frags (idx+(getHoleNum e1)) comps e2)
    Tuple e1 e2 -> Tuple (fillHoles frags idx comps e1) (fillHoles frags (idx+(getHoleNum e1)) comps e2)
    Enumeration.Fun name params e -> Enumeration.Fun name (fillParamHoles frags idx comps params) (fillHoles frags (idx+(foldl (+) 0 (map getHoleNum params))) comps e)
    Stream s1 s2 -> Stream (if (strHasHole s1) then exprToStr (fst (nth (nth idx frags) comps)) else s1) (if (strHasHole s2) then exprToStr (fst (nth (nth (idx+(if (strHasHole s1) then 1 else 0)) frags) comps)) else s2)
    Where e es -> Where (fillHoles frags idx comps e) (fillParamHoles frags (idx+(getHoleNum e)) comps es)
    Switch e es -> Switch (fillHoles frags idx comps e) (fillSwitchHoles frags (idx+getHoleNum e) comps es)

replaceTemplate :: String -> String -> String -> Int -> String
replaceTemplate [] _ _ _  = ""
replaceTemplate (x:xs) code name idx
    | (x == '?' && idx == 1) = code ++ (replaceTemplate xs code name (idx+1))
    | (x == '?' && idx == 2) = name ++ xs
    | otherwise = x:(replaceTemplate xs code name idx)

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

-- | The function 'testMap' is for filters testing
-- The first argument is of type 'String' which is the code to be executed
-- In this function, we call the 'runhaskell' in the shell command and get the
-- output to compare whether they have the same behavior as the built-in filter
testMap :: String -> IO Bool
testMap codeFile = do
    let p = ((\x -> x > 75) :: Int -> Bool)
    testCase <- (randomList 10 (1,100)) -- Some magic numbers here
    (exitCode,testRes,errMsg) <- readProcessWithExitCode "runhaskell" ["-XExistentialQuantification",codeFile,(listToStr testCase)] ""
    case exitCode of
        ExitSuccess -> do
                        putStrLn $ show (map (\x->if x<50 then x+100 else x) testCase)
                        let expcRes = show (map p (map (\x->if x<50 then x+100 else x) testCase))
                        let cleanRes = deleteAt ((Data.List.length testRes) - 1) testRes
                        putStrLn $ expcRes
                        putStrLn $ cleanRes
                        return (cleanRes == expcRes)
        ExitFailure i -> do 
                        --putStrLn $ errMsg
                        readProcessWithExitCode "rm" [codeFile] ""
                        return False

testAppend :: String -> IO Bool
testAppend codeFile = do
    let p = ((\x -> x > 75) :: Int -> Bool)
    testCase <- (randomList 10 (1,100)) -- Some magic numbers here
    (exitCode,testRes,errMsg) <- readProcessWithExitCode "runhaskell" ["-XExistentialQuantification",codeFile,(listToStr testCase)] ""
    case exitCode of
        ExitSuccess -> do
                        putStrLn $ show (map (\x->if x<50 then x+50 else x) testCase)
                        let l = (map (\x->if x<50 then x+50 else x) testCase)
                        let expcRes = show (l++l )
                        let cleanRes = deleteAt ((Data.List.length testRes) - 1) testRes
                        putStrLn $ expcRes
                        putStrLn $ cleanRes
                        return (cleanRes == expcRes)
        ExitFailure i -> do 
                        --putStrLn $ errMsg
                        -- readProcessWithExitCode "rm" [codeFile] ""
                        return False

testDouble  :: String -> IO Bool
testDouble codeFile = do
    let p = ((\x -> x > 75) :: Int -> Bool)
    testCase <- (randomList 10 (1,100)) -- Some magic numbers here
    (exitCode,testRes,errMsg) <- readProcessWithExitCode "runhaskell" ["-XExistentialQuantification",codeFile,(listToStr testCase)] ""
    case exitCode of
        ExitSuccess -> do
                        let l = (map (\x->if x<50 then x+100 else x) testCase)
                        putStrLn $ show (l)
                        let expcRes = show (map (\x->x*2) l)
                        let cleanRes = deleteAt ((Data.List.length testRes) - 1) testRes
                        putStrLn $ expcRes
                        putStrLn $ cleanRes
                        return (cleanRes == expcRes)
        ExitFailure i -> do 
                        --putStrLn $ errMsg
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
iteration :: [[Int]] -> Int -> String -> String -> [(Expr,TypeExpr)] -> Expr -> [(Expr,TypeExpr)] -> IO String
iteration fraglist idx signature sketchHead comps fun freeVar = do
    if (idx > 100000)--length fraglist)
        then return "Failed"
        else do
            let funName = getFunName signature
            file <- readFile "template.hs"
            let program = fillHoles (nth idx fraglist) 0 comps fun
            --putStrLn $ exprToStr program
            if not (validateExpr program freeVar)
                then do
                    putStrLn "check error"
                    iteration fraglist (idx+1) signature sketchHead comps fun freeVar
                else do
                    putStrLn "check success"
                    let sketch = exprToStr program
                    let code = replaceTemplate file (sketchHead++sketch) funName 1
                    --readProcessWithExitCode "rm" [codeFile] ""
                    --fillHoles (nth idx fraglist) 0 newsketch comps
                    let codeFile = funName ++ show idx ++ ".hs"
                    writeFile codeFile code
                    
                    success <- testMap codeFile
                    if success
                        then do 
                            putStrLn code
                            return code
                        else do
                            putStrLn "Wrong program, new iteration..."
                            readProcessWithExitCode "rm" [codeFile] ""
                            iteration fraglist (idx+1) signature sketchHead comps fun freeVar

-- | This is the main function to be executed
main = do
    [f] <- getArgs
    file <- readFile f
    let filelines = getLines file
    let signature = getSignature filelines
    let funName = getFunName signature
    let fun = generateFun signature
    let funType = typeTransform (getFunType signature)
    let sketchHead = ""--funName ++ "::"++(typeToStr funType)++"\n"
    let sketch = sketchHead++(exprToStr fun)
    let freeVar = [(Var "(f x)",Term (TVar "FV")),(Var "x",Term (TVar "FV"))]
    let comps = (getComponents signature freeVar)
    let holeIdx =  assignHoles comps fun
    let assignment = generateAssignments holeIdx
    let program = fillHoles (nth 7 assignment) 0 comps fun
    --putStrLn $ show (nth 0 assignment)
    putStrLn $ exprToStr fun
    putStrLn $ exprToStr program
    --putStrLn $ observe "validInEnv" (show (validInEnv ["True","False"] program))
    --putStrLn $ show (length comps)
    putStrLn $ unlines (printComponents comps)
    putStrLn $ show (holeIdx)
    -- putStrLn $ exprToStr (generateFun (getSignature filelines))
    reslines <- iteration assignment 0 signature sketchHead comps fun freeVar
    putStrLn reslines
    --putStrLn $ typeToStr (typeParser (lexer "[Int->Int]"))