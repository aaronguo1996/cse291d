import System.Environment
import StreamFusion
import Helper

maps :: (a -> b) -> Stream a -> Stream b
maps f (Stream next0 s0) = Stream next s0
    where
        next s = case next0 s of
                    Done -> Done
                    Skip s1 -> Skip (s1)
                    Yield x s1 -> Yield (f x) (s1)

main = do
    [f] <- getArgs
    let l = (strToList f)
    let s = (randomStream l)
    --putStrLn $ show s
    putStrLn $ show (unstream (maps (\x-> x > 75) s))
