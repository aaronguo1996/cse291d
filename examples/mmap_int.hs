{- map function -}

mmap_int :: (Int -> Int) -> [Int] -> [Int]
mmap_int f l = case l of
    []  -> []
    h:t -> (f h):(mmap_int f t)