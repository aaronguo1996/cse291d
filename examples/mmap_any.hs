{- map function -}

mmap_any :: (a -> b) -> [a] -> [b]
mmap_any f l = case l of 
    []  -> []
    h:t -> (f h):(mmap_any f t)