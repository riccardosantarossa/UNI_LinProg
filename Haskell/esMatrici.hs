
--1. controlla se tutte le righe della matrice (lista di liste) hanno la stessa lunghezza
{-matrixDim :: [[Int]] -> (Int,Int)
matrixDim [[]] = (0,0)
matrixDim [[xs]] = (1, length [xs])
matrixDim (xs:xss) = matrixDim1 xss 1
    where
        matrixDim1 (ys:yss) row = (row, length [ys]) : matrixDim1 (row +1) yss -}

matrixDim :: [[Int]] -> (Int, Int)
matrixDim [] = (0, 0)
matrixDim xs
  | null (head xs) = (length xs, 0)
  | otherwise      = (length xs, length (head xs))

-----------------------------------------------------------------

--2. calcola il vettore della somma dei valori delle colonne della matrice (M memorizzata per colonne come lista di liste)
colSums :: [[Int]] -> [Int]
colSums xss = map sum xss

-----------------------------------------------------------------

--4 data M implementata per righe calcola le coppie (minimo, massimo) delle colonne della matrice

--estraggo le colonne dalla matrice espressa per righe
extractCols:: [[Int]] -> [[Int]]
extractCols ([]:_) = []
extractCols xss = map head xss : extractCols (map tail xss)

--trovo massimo e minimo di ogni colonna
getMinMax :: [[Int]] -> [(Int,Int)]
getMinMax = map (\col -> (minimum col, maximum col))

--compongo le due funzioni con l'operatore di composizione "."
colMinMax :: [[Int]] -> [(Int,Int)]
colMinMax = getMinMax . extractCols 

-----------------------------------------------------------------

--9 scrivere una funzione che traspone una matrice (M memorizzata per righe)
trasp:: [[Int]] -> [[Int]]
trasp ([]:_) = []
trasp xss = map head xss : trasp (map tail xss)

-----------------------------------------------------------------
