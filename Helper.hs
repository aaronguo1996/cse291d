module Helper where

import StreamFusion
import System.Random
import Data.List

 -- | Helper function to build a list from a string with space as separators
 -- The first argument for this function is of type 'String'
strToList :: String -> [Int]
strToList str = map read $ words str

-- | Helper function to covert a list into a string separated by space
-- The first argument for this function is of type '[Int]' which is the list
listToStr :: [Int] -> String
listToStr []     = ""
listToStr [x]    = show x
listToStr (x:xs) = (show x) ++ " " ++ (listToStr xs)

-- | Helper function to delete the element from a list in the nth position
-- The first argument is of type 'Int' which is the position n
-- The second argument is of type '[a]' which is the list we delete from
deleteAt :: Int -> [a] -> [a]
deleteAt 0 (x:xs) = xs
deleteAt n (x:xs) | n >= 0 = x : (deleteAt (n-1) xs)
deleteAt _ _ = error "index out of range"

-- | Helper function which returns the nth element in a list
-- The first argument is of type 'Int' which is the position n
-- The second argument is of type '[a]' which is the list we select from
nth :: Int -> [a] -> a
nth 0 (x : _ ) = x
nth n (_ : xs) = nth (n - 1) xs

-- | Helper function to generate a list of random integers for testing
-- The first argument is of type 'Int' which denotes length of generated list
-- The second argument is of type '(Int,Int)' which is the range of list elmts
randomList :: Int -> (Int,Int) -> IO([Int])
randomList 0 _    = return []
randomList n bnds = do
    r  <- randomRIO bnds
    rs <- randomList (n-1) bnds
    return (r:rs) 
--randomList :: (Random a) => (a,a) -> Int -> StdGen -> [a]
--randomList bnds n = take n . randomRs bnds

-- | a helper function to add a sequence of up to element to each of the given 
-- lists and get a new list of lists with their multiplication
-- The first argument is the added element
-- The second argument is the given lists
appendTo :: Int -> [[Int]] -> [[Int]]
appendTo (-1) tails = []
appendTo elmt tails = (appendTo (elmt - 1) tails) ++ (Prelude.map ((:) elmt) tails)

-- | a helper function for getHoleIdx to generate a sequence of integers
-- The first argument is an integer which is the sequence top
sequenceTo :: Int -> [[Int]]
sequenceTo top
    | top > 0   = (sequenceTo (top-1)) ++ [[top]]
    | otherwise = [[0]]

randomStream :: [Int] -> Stream Int
randomStream l = Stream next l
  where
    next [] = Done
    next (x:xs)
        | x < 50 = Skip ((x+50):xs)
        | otherwise = Yield x xs

substr2 :: String -> Int -> Int -> String
substr2 ""  _ _ = ""
substr2 str i j = drop i $ take j str