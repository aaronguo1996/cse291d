nonsense_2 :: [Int] -> [Int]
nonsense_2 l = case l of
    []  ->  []
    h:t     | (h<7 && h*h>4)    -> h:(nonsense_2 t)
            | otherwise         -> nonsense_2 t