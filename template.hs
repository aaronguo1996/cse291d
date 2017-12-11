import System.Environment
import StreamFusion
import Helper

?

main = do
    [f] <- getArgs
    let l = (strToList f)
    let s = (randomStream l)
    --putStrLn $ show s
    putStrLn $ show (unstream (? (\x->x>75) s))