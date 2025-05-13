--esempi di prova

--funzione fattoriale
fact f =
    if f <= 1 then
        1
    else
        f * fact (f-1)

--coefficiente binomiale
binom n k =
    fact n / (fact k * fact (n-k))


--lista di combinazioni
--binomListMAP num = 
   --TODO

--binomListREC num lst =
    
main = do
    --print (fact 5)
    --print (binom 5 3)
    --print (binomListMAP 5)
    --print binomList