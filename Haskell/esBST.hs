{-# LANGUAGE DatatypeContexts #-}
import qualified Data.List

data (Ord a, Eq a) => Tree a = Nil | Node (Tree a) a (Tree a) 
 deriving Show

-----------------------------------------------------------------

--1 somma dei valori di un BST 
sommaBST :: (Num a, Ord a) => Tree a -> a
sommaBST Nil = 0
sommaBST (Node lf v rt) = v + sommaBST lf + sommaBST rt

-----------------------------------------------------------------

--2 somma dei valori dispari di un BST
oddSumBST :: Integral a => Tree a -> a
oddSumBST Nil = 0
oddSumBST (Node lf v rt)
 | odd v = v + oddSumBST lf + oddSumBST rt
 | even v = 0 + oddSumBST lf + oddSumBST rt

-----------------------------------------------------------------

--4 determinare se un valore è contenuto o meno in un BST
bstElem :: Ord a => Tree a -> a -> Bool
bstElem Nil _ = False
bstElem (Node lf v rt) x
 | x == v = True
 | x > v = bstElem rt x
 | x < v = bstElem lf x

-----------------------------------------------------------------

