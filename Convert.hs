import System.Environment
import System.IO
import System.Random
import Control.Monad
import Data.List
import Enumeration
import StreamFusion
import Verification

getLines :: String -> [String]
getLines file = lines file

getHoleByLine :: String -> Int
getHoleByLine line = case line of
                       []   -> 0
                       x:xs -> if x == '?'
                               then 1 + getHoleByLine xs
                               else getHoleByLine

getHoles :: [String] -> Int
getHoles fileline = Prelude.foldl getHoleByLine 0 fileline

-- a helper function for getHoleIdx to generate a sequence of integers
-- The first argument is an integer which is the sequence top
sequenceTo :: Int -> [[Int]]
sequenceTo top = if top > 1
                 then (sequenceTo (top-1)) ++ [[top]]
                 else [[1]]

-- a helper function to add a sequence of up to element to each of the given 
-- lists and get a new list of lists with their multiplication
-- The first argument is the added element
-- The second argument is the given lists
appendTo :: Int -> [[Int]]
appendTo elmt tails = (appendTo (elmt - 1) tails) ++ (Prelude.map ((:) elmt) tails)

-- the function 'getHoleIdx' assign holes by the number of holes and 
-- number of the candidates
-- The first argument is an Int denotes the number of holes
-- The second argument is an Int denotes the number of candidates
getHoleIdx :: Int -> Int -> [[Int]]
getHoleIdx holes cands =
    if holes == 1
    then sequenceTo cands
    else appendTo cands (getHoleIdx (holes - 1) cands)

-- the function 'fillHoles' replace the holes in the program by specific code
-- fragments according to their assigned indices
fillHoles :: [Int] -> String -> String
fillHoles idx line = case line of
                     [] -> []
                     x:xs -> if x == '?'
                             then (exprToStr (expand idx ["s1"])) ++ (fillHoles idx xs)
                             else x:(fillHoles idx xs)

convertQM :: Int -> [String] -> [String]
convertQM idx filelines = case filelines of
                          [] -> []
                          x:xs -> (replace idx x):(convertQM (mod (idx+1) 4) xs)

testFilter :: ((a -> Bool) -> Stream a -> Stream a) -> Bool
testFilter f = unstream (f (odd) (stream [1,2,3,4,5,6,7,8,9,0]))) == (Prelude.filter (odd) [1,2,3,4,5,6,7,8,9,0]

iteration :: Int -> [String] -> [String]
iteration idx filelines = if testFilter filters
                          then filelines
                          else iteration (idx+1) (convertQM (idx+1) filelines)

main = do
    [f] <- getArgs
    file <- readFile f
    putStrLn (unlines (convertQM (mod num 4) (getLines file)))
