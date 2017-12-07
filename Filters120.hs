module Filters120(filters) where

import System.Environment
import StreamFusion
import Helper

filters :: (Int -> Bool) -> Stream Int -> Stream Int
filters f (Stream next0 s0) = Stream next s0
    where
        next s = case next0 s of
                    Done -> Skip (s1)
                    Skip s1 -> Yield (f x) (s1)
                    Yield x s1 | f x -> Yield x (s1)
                               | otherwise -> Done

main = do
    [f] <- getArgs
    let l = (strToList f)
    putStrLn $ show (unstream (filters (\x-> x > 10) (stream l)))
