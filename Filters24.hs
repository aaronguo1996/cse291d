module Filters24(filters) where

import System.Environment
import StreamFusion
import Helper

filters :: (Int -> Bool) -> Stream Int -> Stream Int
filters f (Stream next0 s0) = Stream next s0
    where
        next s = case next0 s of
                    Done -> Done
                    Skip s1 -> Skip (s1)
                    Yield x s1 | f x -> Yield x (s1)
                               | otherwise -> Done

main = do
    [f] <- getArgs
    let l = (strToList f)
    let s = (randomStream l)
    --putStrLn $ show s
    putStrLn $ show (unstream (filters (\x-> x > 75) s))
