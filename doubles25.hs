import System.Environment
import StreamFusion
import Helper

doubles::Stream a->Stream a
doubles 	(Stream next_1 s_1)=(Stream next s_1)
  where
	next 	s_1=case (next_1 s_1) of
		Done -> Done
		Skip (s_1) -> Skip (s_1)
		Yield y (s_1) -> Done


main = do
    [f] <- getArgs
    let l = (strToList f)
    let s = (randomStream l)
    --putStrLn $ show s
    putStrLn $ show (unstream (doubles s))