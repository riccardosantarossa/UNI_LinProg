{-# LANGUAGE DatatypeContexts #-}

--27/06/2018
--verificare che una lista di coppie non contenga due coppie con lo stesso primo elemento
checkDuplicates :: Eq a => [a] -> Bool
checkDuplicates [] = False
checkDuplicates (x:xs) = elem x xs || checkDuplicates xs

firstDuplicate :: Eq a => [(a, b)] -> Bool
firstDuplicate xs = checkDuplicates (map fst xs)

--11/06/2018
--contare le occorrenze di un elemento in un BST
data (Ord a, Eq a) => Tree a = Nil | Node (Tree a) a (Tree a)
 deriving Show

countBst :: (Num a, Ord t) => t -> Tree t -> a
countBst x Nil = 0
countBst x (Node lf v rt)
    | (x == v) = 1 + countBst x lf + countBst x rt
    | otherwise = 0 + countBst x lf + countBst x rt

