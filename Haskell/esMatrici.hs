
--2. calcola il vettore della somma dei valori delle colonne della matrice (M memorizzata per colonne come lista di liste)
colSums :: [[Int]] -> [Int]
colSums xss = map sum xss

--input: let matr = [[1,2,3],[4,5,6],[7,8,9]]
--output colSums matr -> [6,15,24]
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

--input: let matr = [[1,2,3],[4,5,6],[7,8,9]]
--output colMinMax matr -> [(1,7), (2,8), (3,9)]
-----------------------------------------------------------------

--9 scrivere una funzione che traspone una matrice (M memorizzata per righe)
trasp:: [[Int]] -> [[Int]]
trasp ([]:_) = []
trasp xss = map head xss : trasp (map tail xss)

--input: let matr = [[1,2,3],[4,5,6],[7,8,9]]
--output trsp matr -> [[1,4,7], [2,5,8], [3,6,9]]
-----------------------------------------------------------------
