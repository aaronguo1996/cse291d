import System.Environment
import StreamFusion
import Helper

?

main = do
    [f] <- getArgs
    let l = (strToList f)
    let s = (randomStream l)
    let p = ((\x -> x > 75) :: Int -> Bool)
    --putStrLn $ show s
    putStrLn $ show (unstream (? s))