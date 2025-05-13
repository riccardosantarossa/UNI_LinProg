
matrix_dim :: [[Int]] -> (Int, Int)
matrix_dim [] = (0,0)
matrix_dim (r:rs)
    | stessaLunghezza (r:rs) = (length (r:rs), length r)
    | otherwise (-1,-1)
    where
      stessaLunghezza [] = True
      stessaLunghezza [x] = True
      stessaLunghezza (x:y:xs) = length x == length y && stessaLunghezza (y:xs)

