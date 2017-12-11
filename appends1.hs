import System.Environment
import StreamFusion
import Helper

appends::Stream a->Stream a->Stream a
appends 	(Stream next_1 s_1) 	(Stream next_3 s_3)=(Stream next ((s_1,s_3),True))
  where
	next 	((x,s_1),True)=case (next_1 x) of
		Done -> Done
		Skip (s_1) -> Done
		Yield x (s_1) -> Done
 	next 	((x,s_1),True)=case (next_3 x) of
		Done -> Done
		Skip (s_1) -> Done
		Yield x (s_1) -> Skip (((x,s_1),True))


main = do
    [f] <- getArgs
    let l = (strToList f)
    --putStrLn $ show s
    putStrLn $ show (unstream (appends (stream l) (stream l)))