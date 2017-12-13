import System.Environment
import StreamFusion
import Helper

ids 	(Stream next_1 s_1)=(Stream next s_1)
  where
	next 	s_1=case (next_1 s_1) of
		Done -> Done
		Skip (s_1) -> Skip (s_1)
		Yield x (s_1) -> Yield x (s_1)


main = do
    [f] <- getArgs
    let l = (strToList f)
    let s = (randomStream l)
    let p = ((\x -> x > 75) :: Int -> Bool)
    --putStrLn $ show s
    putStrLn $ show (unstream (ids s))