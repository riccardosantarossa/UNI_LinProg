{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant bracket" #-}

--LISTA PRODOTTO CARTESIANO DI DUE LISTE

--versione listComprehension
coppie xs ys = [(x,y) | x <- xs, y <- ys]

--versione Monadi
coppie2 xs ys = do
        x <- xs
        y <- ys
        return (x,y)

--soluzione ricorsiva semplice
{-
coppieRec [] ys = []
coppieRec [x:xs] ys = coppieAux x ys ++ coppieRec xs ys 
	where coppieAux x [] = []
	      coppieAux x [y:ys] = (x,y) : coppieAux x ys
-}

--soluzione con folding
coppieFold xs ys = foldr (\ x zs -> (map (\ y -> (x,y)) ys) ++ zs) [] xs


--VERIFICARE SE DUE LISTE SONO PERMUTAZIONI UNA DELL'ALTRA
perm [] [] = True
perm [] _ = False
perm (x:xs) ys = case remove x ys of
                   Nothing -> False
                   Just ys1 -> perm xs ys1

remove x [] = Nothing
remove x (y:ys) | (x == y) = Just ys
                | otherwise = case remove x ys of
                                Nothing -> Nothing
                                Just ys1 -> Just (y : ys1)


--COSTRUIRE UNA LISTA DI LISTE CHE CONTIENE TUTTE LE PERMUTAZIONI DELLA LISTA IN INPUT

listPerm [] = [[]]
listPerm (x:xs) = ins x (listPerm xs)

ins x [] = []
ins x (ys:yss) = (insert x ys) ++ ins x yss

--con la Monade DO
listPerm2 [] = [[]]
listPerm2 (x:xs) = do ys <- listPerm2 xs
                      insert x ys

insert x [] = [[x]]
insert x (y:ys) = (x:y:ys) : (map (\ zs -> y:zs) (insert x ys))


--DETERMINARE SE UNA MATRICE è QUADRATA (matrice come lista di liste)

--funzione length ridefinita
len xs = foldr (\ _ n -> n+1) 0 xs


--funzione standard
squareMatrix xss = checkLen (len xss) xss
checkLen _ [] = True
checkLen n (xs:xss) | (n == len xs) = checkLen n xss
                    | otherwise = False

--funzione usando il FOLD
squareMatrixFOLD xss = checkLenFOLD (len xss) xss
checkLenFOLD n xss = foldr (\ xs b -> (n == len xs) && b) True xss