{- sum -}

msum :: [Int] -> Int
msum l = go l 0
    where 
        go x y = case x of 
            []  -> y
            h:t -> go t (y+h)