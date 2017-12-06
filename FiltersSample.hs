import StreamFusion;

filters :: (a->Bool)->Stream a->Stream a
filters p (Stream next0 s0) = Stream next s0
    where
        next s = case next0 s of
                   Done -> Done
                   Skip s' -> Skip s'
                   Yield x s' | p x -> Yield x s'
                              | otherwise -> Skip s'