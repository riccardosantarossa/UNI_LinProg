
--CON MAP e FILTER
--funzione che applica una funzione ad un elemento
gapp [] x = Nothing
gapp ((x1,y):xys) x | (x1 == x) = Just y 
                    | otherwise = gapp xys x

--applico la funzione a tutta la lista data
gappListAUX graph xs = map (gapp graph) xs     

filter2 [] = []
filter2 (Nothing : xs) = filter2 xs
filter2 (Just x : xs) = x : filter2 xs

gappList graph xs = filter2 (gappListAUX graph xs)


--data una funzione, costruire il grafo delle copppie (x, f(x))
buildGraphAux f [] = []
buildGraphAux f (x:xs) = (x, f x) : (buildGraphAux f xs)

buildGraph f = buildGraphAux f [0..]

--versione con MAP 
buildGraph2 f xs = map (\ x -> (x, f x)) [0..] 

--versione con ListComphrension
buildGraph3 f = [(x, f x) | x <- [0..]]

--versione con FOLD
buildGraph4 f xs = foldr (\ x gs -> (x, f x) : gs) [] xs


--combinare due liste per ottenere i numeri Naturali
combine [] xs  = xs
combine xs [] = xs
combine (x:xs) (y:ys) = x : y : combine xs ys

listIntegers = combine [0..] (map (\ x -> -x) [1..])


