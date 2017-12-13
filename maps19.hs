import System.Environment
import StreamFusion
import Helper

maps 	f_1 	(Stream next_3 s_3)=(Stream next s_3)
  where
	next 	s_3=case (next_3 s_3) of
		Done -> Done
		Skip (s_3) -> Skip (s_3)
		Yield x (s_3) -> Yield (f_1 x) (s_3)


main = do
    [f] <- getArgs
    let l = (strToList f)
    let s = (randomStream l)
    let p = ((\x -> x > 75) :: Int -> Bool)
    --putStrLn $ show s
    putStrLn $ show (unstream (maps p s))