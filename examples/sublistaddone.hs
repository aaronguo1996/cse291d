{- add one to each element in the sublist -}

sublistaddone :: [[Int]] -> [[Int]]
sublistaddone l = case l of 
    []  -> []
    h:t -> (addone h):(sublistaddone t)
    where
        addone l = case l of 
            []  -> []
            h:t -> (h+1):(addone t)