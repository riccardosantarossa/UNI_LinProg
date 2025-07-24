data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
  deriving (Eq, Show)
-----------------------------------------------------------------

--1 dati 4 QuadTree, ne costruisce uno combinandoli
buildNSimplify :: (Eq a, Show a) => QT a -> QT a -> QT a -> QT a -> QT a
buildNSimplify as ad bs bd =
  case (as, ad, bs, bd) of
    (C a, C b, C c, C d)  
      | a == b && b == c && c == d -> C a
    _ -> Q as ad bs bd 

-----------------------------------------------------------------

--4 dato un QuadTree, conto il numero minimo di pixel che rappresenta

--conto i pixel tenendo conto della "profondità" dell'immagine
pixelsAtLevel :: (Eq a, Show a) => Int -> QT a -> Int
pixelsAtLevel depth (C _) = 4 ^ depth
pixelsAtLevel depth (Q as ad bs bd) = pixelsAtLevel (depth + 1) as + pixelsAtLevel (depth + 1) ad + pixelsAtLevel (depth + 1) bs + pixelsAtLevel (depth + 1) bd

--combinazione delle funzioni
howManyPixels :: (Eq a, Show a) => QT a -> Int
howManyPixels = pixelsAtLevel 0

