module StreamFusion where

data Step a s = Done
              | Yield a s
              | Skip s

data Stream a = forall s. Stream (s -> Step a s) s

stream :: [a] -> Stream a
stream xs0 = Stream next xs0
    where
        next [] = Done
        next (x:xs) = Yield x xs

unstream :: Stream a -> [a]
unstream (Stream next0 s0) = unfold s0
    where
        unfold s = case next0 s of
            Done -> []
            Skip s' -> unfold s'
            Yield x s' -> x:unfold s' 
