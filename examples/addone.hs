{- increment each element in the list -}

addone :: [Int] -> [Int]
addone l = case l of 
    []  -> []
    h:t -> (h+1):(addone t)