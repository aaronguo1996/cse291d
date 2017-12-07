module Filters162(filters) where

import System.Environment
import StreamFusion
import Helper

filters :: (Int -> Bool) -> Stream Int -> Stream Int
filters f (Stream next0 s0) = Stream next s0
    where
        next s = case next0 s of
                    Done -> Yield x (s1)
                    Skip s1 -> Yield x (s1)
                    Yield x s1 | f x -> Done
                               | otherwise -> Yield x (s1)

main = do
    [f] <- getArgs
    let l = (strToList f)
    putStrLn $ show (unstream (filters (\x-> x > 10) (stream l)))
