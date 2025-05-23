--1. rimuove dalla lista gli elementi in posizione pari partendo da 1
removeEven :: [Int] -> [Int]
removeEven [] = []
removeEven [x] = [x]
removeEven (x:_:xs) = x : removeEven xs 

-----------------------------------------------------------------

--2. calcola la somma degli elementi in posizione dispari di una lista
oddSum :: [Int] -> Int
oddSum [] = 0
oddSum [x] = 0
oddSum (_:x:xs) = x + oddSum xs

-----------------------------------------------------------------

--3.quicksort per dati interi
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) =
    quicksort[ y | y <- xs, y < x] ++ (x : quicksort[ y | y <- xs, x <= y])

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

-----------------------------------------------------------------

--5. costruisce una lista di coppie con l'elemento e la somma di tutti gli elementi dopo di lui
pairedSum :: [Int] -> [(Int, Int)]
pairedSum [] = [(0,0)]
pairedSum [x] = [(x,0)]
pairedSum (x:xs) = [(x, foldr (+) 0 xs)] ++ pairedSum xs

-----------------------------------------------------------------

--6. costruisce una lista di coppie in cui il primo elemento della coppia è l'elemento stesso il secondo è la somma dei precedenti.
paired :: [Int] -> [(Int, Int)]
paired xs = paired1 xs 0
  where
    paired1 [] _ = []
    paired1 (y:ys) acc = (y, acc) : paired1 ys (acc + y)

-----------------------------------------------------------------

--7.Sottraggo l'elemento minimo da ogni elemento della lista
shiftToZero :: [Int] -> [Int]
shiftToZero [] = []
shiftToZero [x] = [0]
shiftToZero xs = map (\n -> n-minimum xs) xs

-----------------------------------------------------------------


 