{- cumulative sum -}

mcumsum :: [Int] -> [Int]
mcumsum l = go l 0
    where 
        go x y = case x of 
            []  -> []
            h:t -> (y+h):(go t (y+h))