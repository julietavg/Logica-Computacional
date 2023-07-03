module Trees where

data BinTreeN node = Void
            | Node node (BinTreeN node) (BinTreeN node)
            deriving (Show,Eq,Ord)

--Suma de los valores que hay en el árbol
treeSum :: (Num a) => BinTreeN a -> a
treeSum Void = 0
treeSum (Node nodo left right) = nodo + treeSum (left) + treeSum(right)

-- Tal que (mapBinTree f x) regresa el árbol que resulta de aplicar la función f a cada
--elemento del árbol x
mapBinTree :: (a -> b) -> BinTreeN a -> BinTreeN b
mapBinTree _ Void = Void
mapBinTree n (Node nodo left right) = Node (n nodo) (mapBinTree n left) (mapBinTree n right)

-- Funcion auxiliar que calculará la altura de un árbol
altura :: BinTreeN a -> Int
altura Void = 0
altura (Node _ l r) = 1 + (max (altura l) (altura r))

--Verifica si el árbol a está balanceado
isBalanced :: BinTreeN a -> Bool
isBalanced Void = True
isBalanced (Node nodo left right) = let alturaIzq = altura left; alturaDer = altura right in if (alturaIzq >= alturaDer) == True
                                                            then if ((alturaIzq - alturaDer) <= 1) == True 
                                                                    then (isBalanced left == True && isBalanced right == True)  
                                                                    else False
                                                            else if ((alturaDer - alturaIzq) <= 1) == True
                                                                    then (isBalanced left == True && isBalanced right == True)  
                                                                    else False


--Tipo de dato que representa un árbol binario que almacena valores
--solo en las hojas del árbol.

data BinTreeL h = Leaf h
                | Fork (BinTreeL h) (BinTreeL h)
                deriving(Show)

--función auxiliar para comparar dos listas
compara :: (Eq a) => [a] -> [a] -> Bool
compara [] [] = True
compara (x:xs) (y:ys) = if (x == y) == True 
                                        then compara xs ys
                                        else False

--Función auxiliar que recorre el arbol en preorden y 
--almacena los valores de las hojas en una lista
preOrder :: BinTreeL a -> [a]
preOrder (Leaf hoja) = [hoja]
preOrder (Fork hojaIzq hojaDer) = preOrder hojaIzq ++ preOrder hojaDer 



-- Verifica si los bordes de los árboles son iguales
isEqualEdge :: (Eq a) => BinTreeL a -> BinTreeL a -> Bool
isEqualEdge arbol1 arbol2 = compara (preOrder arbol1) (preOrder arbol2)

--Tipo de dato que reoresenta un árbol binario que almacena valores
--en las hojas y en nodos internos del árbol.
data BinTree arbolito = Bract arbolito 
               | Branch arbolito (BinTree arbolito) (BinTree arbolito)
               deriving (Show)

--verifica si los árboles tienen la misma estructura
equalsStructures :: (Eq a) => BinTree a -> BinTree a -> Bool
equalsStructures (Bract _) (Bract _) = True
equalsStructures (Branch _ izquierda derecha) (Branch _ izquierda1 derecha1) = if (equalsStructures (izquierda) (izquierda1) == True) == True
                                                                                                            then equalsStructures derecha derecha1
                                                                                                            else False
equalsStructures _ _ = False

--Regresa la lista de los elementos de nivel k del árbol
level :: Int -> BinTree a -> [a]
--Casos base
--Raiz
level 0 (Branch nodo _ _) = [nodo] 
--Hoja
level 0 (Bract nodo) = [nodo]
--Casos para n
level n (Bract nodo) = []
level n (Branch nodo izq der) = level (n-1) (izq) ++ level (n-1) (der)


--Tipo de dato que representa un árbol general
data Tree a = Limb a [Tree a]
            deriving (Show,Eq,Ord)


--regresa el árbol que resulta de añadir una copia del árbol
--a2 a los nodos de a1 que cumplen un predicado p.
branching :: Tree a -> Tree a -> (a -> Bool) -> Tree a
branching  (Limb x l) t p1
            | p1 x = Limb x ([branching a t p1 | a <- l]++ [t])
            |otherwise = Limb x [branching a t p1 | a <- l]

