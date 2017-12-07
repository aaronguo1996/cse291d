module Helper where

strToList :: String -> [Int]
strToList str = map read $ words str

listToStr :: [Int] -> String
listToStr []     = ""
listToStr [x]    = show x
listToStr (x:xs) = (show x) ++ " " ++ (listToStr xs)

deleteAt :: Int -> [a] -> [a]
deleteAt 0 (x:xs) = xs
deleteAt n (x:xs) | n >= 0 = x : (deleteAt (n-1) xs)
deleteAt _ _ = error "index out of range"