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


--09/09/2019
--unire due liste ordinate creando una lista ordinata senza ripetizioni

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x < y = x : mergeLists xs (y:ys)
    | x > y = y : mergeLists (x:xs) ys
    | otherwise = x : mergeLists xs ys


--19/09/2018
--dato un intero n costruire una matrice n*n di soli 0 e 1 alternati a scacchiera

rowStartZero :: Int -> [Int]
rowStartZero n 
 | (n == 0) = []
 | even n = [1] ++ rowStartZero (n-1)
 | otherwise = [0] ++ rowStartZero (n-1)

rowStartOne :: Int -> [Int]
rowStartOne n 
 | (n == 0) = []
 | even n = [0] ++ rowStartOne (n-1)
 | otherwise = [1] ++ rowStartOne (n-1)

--costruisce la matrice supponendo sia memorizzata per righe e che in posizione [1,1] vi sia uno 0
matrixOneZero :: Int -> [[Int]]
matrixOneZero n = matrixAux n n

matrixAux :: Int -> Int -> [[Int]]
matrixAux n k
 | (k == 0) = []
 | even k = [rowStartOne n] ++ matrixAux n (k-1)
 | otherwise = [rowStartZero n] ++ matrixAux n (k-1)


