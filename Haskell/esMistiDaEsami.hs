{-# LANGUAGE DatatypeContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}


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
    | x == v = 1 + countBst x lf + countBst x rt
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
 | even n = 1 : rowStartZero (n-1)
 | otherwise = 0 : rowStartZero (n-1)

rowStartOne :: Int -> [Int]
rowStartOne n
 | (n == 0) = []
 | even n = 0 : rowStartOne (n-1)
 | otherwise = 1 : rowStartOne (n-1)

--costruisce la matrice supponendo sia memorizzata per righe e che in posizione [1,1] vi sia uno 0
matrixOneZero :: Int -> [[Int]]
matrixOneZero n = matrixAux n n

matrixAux :: Int -> Int -> [[Int]]
matrixAux n k
 | (k == 0) = []
 | even k = rowStartOne n : matrixAux n (k-1)
 | otherwise = rowStartZero n : matrixAux n (k-1)


--esame 02/07/2019 
--data una matrice quadrata memorizzata per righe togliere prima riga e prima colonna
getSubmatrix:: [[Int]] -> [[Int]]
getSubmatrix = removeFirstColumn . removeFirstRow 

removeFirstRow:: [[Int]] -> [[Int]]
removeFirstRow xss = drop 1 xss 

removeFirstColumn:: [[Int]] -> [[Int]]
removeFirstColumn xss = map (drop 1) xss

--estrarre gli elementi della diagonale da una matrice quadrata memorizzata per righe
getDiag:: [[Int]] -> [Int]
getDiag m = getDiagAux m 0

getDiagAux:: [[Int]] -> Int -> [Int]
getDiagAux [] _ = []
getDiagAux (xs:xss) idx = (xs !! idx) : getDiagAux xss (idx + 1)


--esame 15/06/2021
--funzione che fattorizza un numero
fattorizzazione:: Int -> [Int]
fattorizzazione n = fatt n 2
 where
    fatt n d
     | n < 2 = []
     | (n `mod` d == 0) = d : fatt (n `div` d) d
     | otherwise = fatt n (d + 1)


--esame 05/07/2021
--funzione che determina se una lista è palindroma
palindromeList:: [Int] -> Bool
palindromeList lst = (lst == reverse lst)

--verificare che una matrice sia simmetrica rispetto all'asse verticale -> righe palindrome 
verticalSimmetry:: [[Int]] -> Bool
verticalSimmetry [] = True
verticalSimmetry (row:rows)
 | palindromeList row = verticalSimmetry rows
 | otherwise = False

--verificare che una matrice sia simmetrica rispetto all'asse orizzontale -> matrice palindroma
horizontalSimmetry:: [[Int]] -> Bool
horizontalSimmetry m = (m == reverse m)

