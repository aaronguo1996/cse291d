{- some nonsense func -}

nonsense_1 :: [Int] -> [Int]
nonsense_1 l = case l of 
    []  -> []
    h:t -> (f h):(nonsense_1 t)
        where 
            f x = x * x - 66 * x + 1000 