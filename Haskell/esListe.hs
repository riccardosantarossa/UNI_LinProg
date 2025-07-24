--1. rimuove dalla lista gli elementi in posizione pari partendo da 1
removeEven :: [Int] -> [Int]
removeEven [] = []
removeEven [x] = [x]
removeEven (x:_:xs) = x : removeEven xs 

--input: let lst = [1,15,6,7]
--output: removeEven lst -> [1,6]

-----------------------------------------------------------------

--2. calcola la somma degli elementi in posizione dispari di una lista da zero
oddSum :: [Int] -> Int
oddSum [] = 0
oddSum [x] = 0
oddSum (_:x:xs) = x + oddSum xs

--input: let lst = [1,15,6,7]
--output: oddSum lst -> 22
-----------------------------------------------------------------

--3.quicksort per dati interi
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) =
    quicksort[ y | y <- xs, y < x] ++ (x : quicksort[ y | y <- xs, x <= y])

--input: let lst = [1,15,6,7]
--output: quicksort lst -> [1,6,7,15]
-----------------------------------------------------------------

--4. restituisce i due numeri dispari più piccoli di una lista
dueDispari :: [Int] -> (Int, Int)
dueDispari arr = 
    minOdd (quicksort arr)

minOdd :: [Int] -> (Int, Int)
minOdd (x:y:xs)
    | odd x && odd y = (x,y)
    | odd x = minOdd' x xs
    | otherwise = minOdd (y:xs)
    where
        minOdd' first (x:xs)
            | odd x = (first, x)
            | otherwise = minOdd' first xs

--input: let lst = [1,15,6,7]
--output: dueDispari lst -> (1,7)
-----------------------------------------------------------------

--5. costruisce una lista di coppie con l'elemento e la somma di tutti gli elementi dopo di lui
pairedSum :: [Int] -> [(Int, Int)]
pairedSum [] = [(0,0)]
pairedSum [x] = [(x,0)]
pairedSum (x:xs) = [(x, foldr (+) 0 xs)] ++ pairedSum xs

--input: let lst = [1,15,6,7]
--output: pairedSum lst -> [(1,28),(15,13), (6,7), (7,0)]
-----------------------------------------------------------------

--6. costruisce una lista di coppie in cui il primo elemento della coppia è l'elemento stesso il secondo è la somma dei precedenti.
paired :: [Int] -> [(Int, Int)]
paired xs = paired1 xs 0
  where
    paired1 [] _ = []
    paired1 (y:ys) acc = (y, acc) : paired1 ys (acc + y)

--input: let lst = [1,15,6,7]
--output: paired lst -> [(1,0), (15,1), (6,16), (7,22)]
-----------------------------------------------------------------

--7.Sottraggo l'elemento minimo da ogni elemento della lista
shiftToZero :: [Int] -> [Int]
shiftToZero [] = []
shiftToZero [x] = [0]
shiftToZero xs = map (\n -> n-minimum xs) xs

--input: let lst = [1,15,6,7]
--output: shiftToZero lst -> [0,14,5,6]
-----------------------------------------------------------------


 