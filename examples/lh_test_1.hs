{- get all the elements in a list that are larger than their successors-}

lh_test_1 :: [Int] -> [Int]
lh_test_1 l = case l of
    []  -> []
    h:t -> case t of
        []      -> h:(lh_test_1 t)
        hh:tt   | hh<h      -> h:(lh_test_1 t)
                | hh>=h     -> lh_test_1 t