--Definizione del tipo QuadTree
data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
  deriving (Eq, Show)

--Definizione del tipo Matrice
data Mat a = Mat {
  nexp :: Int,     
  mat  :: QT a    
} deriving (Eq, Show)


--1 controllo se una matrice è triangolare inferiore
--Controlla se tutti i valori in un QT sono zero
isZeroTree :: (Eq a, Num a) => QT a -> Bool
isZeroTree (C x) = x == 0
isZeroTree (Q as ad bs bd) =
  isZeroTree as && isZeroTree ad && isZeroTree bs && isZeroTree bd

--Verifica se il QT rappresenta una matrice triangolare inferiore
triangolareInferioreQT :: (Eq a, Num a) => QT a -> Bool
triangolareInferioreQT (C _) = True  
triangolareInferioreQT (Q as ad bs bd) = --alto a sx, alto a dx, basso a sx, basso a dx
  isZeroTree as &&               
  triangolareInferioreQT ad &&
  triangolareInferioreQT bs &&
  triangolareInferioreQT bd

--Funzione principale
triangolareInferiore :: (Eq a, Num a) => Mat a -> Bool
triangolareInferiore (Mat _ qt) = triangolareInferioreQT qt 


--TEST
-- Costruzione:
-- [ 4 0 ]
-- [ 5 7 ]

qt1 = Q (C 1) (C 0) (C 2) (C 3)
m1 = Mat 1 qt1
--lowertriangular m1
-- Output: True

---------------------------------------------------------------------------------

--Verifica se il QT rappresenta una matrice triangolare superiore
uppertriangularQT :: (Eq a, Num a) => QT a -> Bool
uppertriangularQT (C _) = True
uppertriangularQT (Q as ad bs bd) =
  isZeroTree bs &&              
  uppertriangularQT as &&
  uppertriangularQT ad &&
  uppertriangularQT bd

--Funzione principale
uppertriangular :: (Eq a, Num a) => Mat a -> Bool
uppertriangular (Mat _ qt) = uppertriangularQT qt

-- Costruzione:
-- [ 1 2 ]
-- [ 0 6 ]

qt2 = Q (C 1) (C 2) (C 0) (C 6)
m2 = Mat 1 qt1
--uppertriangular m1
--Output: True