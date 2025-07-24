--esempi di prova

--funzione fattoriale
fact :: Int -> Int
fact 0 = 1
fact n = fact(n - 1) * n 

--input n = 5
--output 120
----------------------------------------------

--coefficiente binomiale
binom :: Int -> Int -> Int
binom n k =
    div (fact n) (fact k * fact (n-k))

--input n = 5, k = 2
--output 10
----------------------------------------------

--lista di combinazioni
binomMap :: Int -> [Int]
binomMap n = map (binom n) [0..n]

binomRec :: Int -> [Int]
binomRec 0 = [1]
binomRec n = bnRec n 0 
    where
        bnRec n k
            | k > n = []
            | otherwise = binom n k : bnRec n (k+1)
    
--input: n = 6
--output [1,6,15,20,15,6,1]