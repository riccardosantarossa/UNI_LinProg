
data Tree a = Nil | Node (Tree a) a (Tree a) 
 deriving Show

-----------------------------------------------------------------

--1 somma dei valori di un BST 
sommaBST :: (Num a, Ord a) => Tree a -> a
sommaBST Nil = 0
sommaBST (Node lf v rt) = v + sommaBST lf + sommaBST rt

-- input = Node (Node Nil 2 Nil) 5 (Node Nil 7 Nil)
-- output = 14
-----------------------------------------------------------------

--2 somma dei valori dispari di un BST
oddSumBST :: Integral a => Tree a -> a
oddSumBST Nil = 0
oddSumBST (Node lf v rt)
 | odd v = v + oddSumBST lf + oddSumBST rt
 | even v = 0 + oddSumBST lf + oddSumBST rt

-- input = Node (Node Nil 2 Nil) 5 (Node Nil 7 Nil)
-- output = 12
-----------------------------------------------------------------

--4 determinare se un valore è contenuto o meno in un BST
bstElem :: Ord a => Tree a -> a -> Bool
bstElem Nil _ = False
bstElem (Node lf v rt) x
 | x == v = True
 | x > v = bstElem rt x
 | x < v = bstElem lf x

-- input = bstElem (Node (Node Nil 2 Nil) 5 (Node Nil 7 Nil)) 7
-- output = True
-----------------------------------------------------------------

--5 inserimento di un valore in un BST
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x Nil = Node Nil x Nil
insertBST x (Node lf v rt) 
 | x == v = Node lf v rt
 | x < v = Node (insertBST x lf) v rt
 | x > v = Node lf v (insertBST x rt)

-- input = insertBST 3 (Node (Node Nil 2 Nil) 5 Nil)
-- output = Node (Node Nil 2 (Node Nil 3 Nil)) 5 Nil
-----------------------------------------------------------------

--6 lista ordinata dei nodi di un BST (visita inOrder)
bst2List :: Ord a => Tree a -> [a]
bst2List Nil = []
bst2List (Node lf v rt) =  bst2List lf ++ [v] ++ bst2List rt

-- input = Node (Node Nil 2 Nil) 5 (Node Nil 7 Nil)
-- output = [2,5,7]
-----------------------------------------------------------------

--7 ordinamento di liste mediante BST
bstFromList :: Ord a => [a] -> Tree a
bstFromList = foldr insertBST Nil 

orderList :: Ord a => [a] -> [a]
orderList xs = bst2List ( bstFromList xs )

-- input = [3,1,4,1,5]
-- output = [1,3,4,5]
-----------------------------------------------------------------
