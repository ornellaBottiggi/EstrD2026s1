-- 1) 1.1

data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

--para testear
celdaEjemplo = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celdaEjemplo1 = CeldaVacia

-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia = 0
nroBolitas color (Bolita color1 c) = unoSisonMismoColor color color1 + nroBolitas color c

unoSisonMismoColor :: Color -> Color -> Int
unoSisonMismoColor c1 c2 = if sonMismoColor c1 c2 then 1 else 0

sonMismoColor :: Color -> Color -> Bool
sonMismoColor Azul Azul = True
sonMismoColor Rojo Rojo = True
sonMismoColor _ _ = False

--Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner color c1 = Bolita color c1

-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
-- Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar color CeldaVacia = CeldaVacia
sacar color (Bolita color1 c) = if sonMismoColor color color1 
then c 
else (Bolita color1 (sacar color c))

-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ celda = celda 
ponerN n c celdaVacia = ponerN (n-1) c (poner c celdaVacia)  
ponerN n c (Bolita _ celda) = ponerN (n-1) c (poner c celda)  

-- 1) 1.2

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

-- para testear
camino1 = Nada (Cofre [Cacharro] (Nada (Cofre [Tesoro] Fin)))
camino2 = Nada (Nada (Nada (Cofre [Cacharro] Fin)))
camino3 = Fin
camino4 = Cofre [Tesoro] Fin
camino5 = Nada (Cofre [Cacharro, Tesoro] (Nada (Cofre [Tesoro] Fin)))
camino6 = Cofre [Cacharro] (Cofre [Tesoro, Tesoro] Fin)

-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro (Fin) = False
hayTesoro (Cofre ls c) = tieneTesoro ls || hayTesoro c
hayTesoro (Nada c) = hayTesoro c 

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (o : os) = esTesoro o || tieneTesoro os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False 

{- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
 Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
Precondición: tiene que haber al menos un tesoro. -}
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Fin) = 0
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre ls c) = if tieneTesoro ls then 0 else 1 + pasosHastaTesoro c

-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool 
hayTesoroEn _ Fin = False 
hayTesoroEn 0 cs = hayTesoroAca cs
hayTesoroEn n cs = avanzarATesoro n cs

avanzarATesoro :: Int -> Camino -> Bool
avanzarATesoro n (Nada c) = hayTesoroEn (n-1) c 
avanzarATesoro n (Cofre _ c) = hayTesoroEn (n-1) c

hayTesoroAca :: Camino -> Bool
hayTesoroAca (Cofre os _) = tieneTesoro os
hayTesoroAca _ = False

-- Indica si hay al menos "n" tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n cs = (0 >= n) || hayTesorosPedidos n cs

hayTesorosPedidos :: Int -> Camino -> Bool
hayTesorosPedidos _ Fin = False
hayTesorosPedidos n (Cofre os c) = alMenosNTesoros (n - (cantidadDeTesoros os)) c
hayTesorosPedidos n (Nada c) = alMenosNTesoros n c

{-Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado.-}
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre _ _ Fin = 0
cantTesorosEntre _ 0 cs = contarTesoro cs
cantTesorosEntre 0 max cs = contarTesoros max cs
cantTesorosEntre min max cs   = avanzar min max cs

contarTesoro :: Camino -> Int
contarTesoro (Nada _) = 0
contarTesoro (Cofre os _) = cantidadDeTesoros os

contarTesoros :: Int -> Camino -> Int
contarTesoros max (Nada c) = cantTesorosEntre 0 (max-1) c
contarTesoros max (Cofre os c) = cantidadDeTesoros os + cantTesorosEntre 0 (max-1) c  

avanzar :: Int -> Int -> Camino -> Int
avanzar min max (Nada c)       = cantTesorosEntre (min-1) (max-1) c 
avanzar min max (Cofre os c) = cantTesorosEntre (min-1) (max-1) c

cantidadDeTesoros :: [Objeto] -> Int
cantidadDeTesoros []         = 0
cantidadDeTesoros (o:os) = (if esTesoro o then 1 else 0) + cantidadDeTesoros os

-- 2) 2.2

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

-- para testear
arbol = NodeT 1 (NodeT 2 (NodeT 2 EmptyT EmptyT) EmptyT) (NodeT 1 EmptyT EmptyT)
arbol1 = NodeT 1 (NodeT 2 (NodeT 6 EmptyT EmptyT) EmptyT) (NodeT 3 EmptyT EmptyT)
arbol2 = (NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT)EmptyT)(NodeT 4 (NodeT 5 EmptyT EmptyT)EmptyT))

-- Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT EmptyT = 0  
sumarT (NodeT n t1 t2) = n + (sumarT t1) + (sumarT t2)

-- Dado un árbol binario devuelve su cantidad de elementos, es decir, 
--el tamaño del árbol (size en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT n t1 t2) = 1 + sizeT t1 + sizeT t2

-- Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n t1 t2) = (NodeT (n*2) (mapDobleT t1) (mapDobleT t2))

-- Dados un elemento y un árbol binario devuelve True si existe un 
-- elemento igual a ese en el árbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT a EmptyT = False
perteneceT a (NodeT n t1 t2) = if (a == n) then True else (perteneceT a t1 || perteneceT a t2)

-- Dados un elemento e y un árbol binario devuelve la cantidad de elementos del 
-- árbol que son iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT a EmptyT = 0
aparicionesT a (NodeT n t1 t2) = if (a == n) 
then 1 + (aparicionesT a t1 + aparicionesT a t2)
else (aparicionesT a t1 + aparicionesT a t2)

-- Dado un árbol devuelve los elementos que se encuentran en sus hojas.
-- NOTA: en este tipo se define como hoja a un nodo con dos hijos vacíos.
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT n EmptyT EmptyT) = [n]
leaves (NodeT n t1 t2) = leaves t1 ++ leaves t2

{- Dado un árbol devuelve su altura.
Nota: la altura de un árbol (height en inglés), también llamada profundidad, es
la cantidad de niveles del árbol. La altura para EmptyT es 0, 
y para una hoja es 1 -}
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT n t1 t2) = 1 + heightT (caminoMasLargo t1 t2)

caminoMasLargo :: Tree a -> Tree a -> Tree a
caminoMasLargo t1 t2 = if sizeT t1 > sizeT t2 then t1 else t2

-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con
-- el derecho, en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT n t1 t2) = NodeT n (mirrorT t2) (mirrorT t1) 

{- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en
modo in-order.
Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo,
luego la raiz y luego los elementos del hijo derecho. -}
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT n t1 t2) = toList t1 ++ [n] ++ toList t2

{- Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El
nivel de un nodo es la distancia que hay de la raíz hasta él. La distancia de la
raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1.
Nota: El primer nivel de un árbol (su raíz) es 0. -}
levelN :: Int -> Tree a -> [a]
levelN n EmptyT = []
levelN 0 (NodeT r t1 t2) = [r]
levelN n (NodeT _ t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

-- Dado un árbol devuelve una lista de listas en la que cada elemento representa
-- un nivel de dicho árbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT r t1 t2) = [r] : listaDeElementosDeNivel(listPerLevel t1) (listPerLevel t2)

listaDeElementosDeNivel :: [[a]] -> [[a]] -> [[a]]
listaDeElementosDeNivel r []  = r
listaDeElementosDeNivel [] r  = r
listaDeElementosDeNivel (ti : tis) (td : tds) = (ti ++ td) : (listaDeElementosDeNivel tis tds) 

-- Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT n t1 t2) = n : elementosDeRama (caminoMasLargo t1 t2)

elementosDeRama :: Tree a -> [a]
elementosDeRama EmptyT = []
elementosDeRama (NodeT n t1 t2) = [n] ++ elementosDeRama t1 ++ elementosDeRama t2


{- 
Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz
hasta cualquiera de los nodos.
ATENCIÓN: se trata de todos los caminos, y no solamente de los maximales (o
sea, de la raíz hasta la hoja), o sea, por ejemplo
todosLosCaminos 
(NodeT 1 
    (NodeT 2 
        (NodeT 3 EmptyT EmptyT)EmptyT)
    (NodeT 4 
        (NodeT 5 EmptyT EmptyT)EmptyT))
= [ [1], [1,2], [1,2,3], [1,4], [1,4,5] ]
OBSERVACIÓN: puede resultar interesante plantear otra función, variación de
ésta para devolver solamente los caminos maximales.-}
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT n t1 t2) = [n] : agregarALista n (todosLosCaminos t1) ++ agregarALista n (todosLosCaminos t2)

agregarALista :: a -> [[a]] -> [[a]]
agregarALista a [] = []
agregarALista a (c : cs) = [[a] ++ c] ++ (agregarALista a cs)

-- plantee esto pero no m sirvio
caminosMaximales :: Tree a -> [[a]]
caminosMaximales EmptyT = []
caminosMaximales (NodeT n t1 t2) = [n : elementosDeRama (t1)] ++ [n : elementosDeRama (t2)]

--2) 2.2
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA deriving Show

-- para testear 
ecuacion = Sum (Valor 2) (Valor 2)
ecuacion1 = Prod ecuacion ecuacion
ecuacion2 = Neg ecuacion1

-- 1. Dada una expresión aritmética devuelve el resultado evaluarla.
eval :: ExpA -> Int
eval (Valor n1) = n1
eval (Sum n1 n2) = (eval n1) + (eval n2)
eval (Prod n1 n2) = (eval n1) * (eval n2)
eval (Neg n1) = (eval n1) * (-1)


{- 2. Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
notación matemática convencional):
a) 0 + x = x + 0 = x
b) 0 * x = x * 0 = 0
c) 1 * x = x * 1 = x
d) - (- x) = x -}

-- para testear
ecuacion3 = Sum (Valor 0) (Valor 2)
ecuacion4 = Prod (Valor 2) (Valor 0)
ecuacion5 = Prod (Valor 2) (Valor 1)
ecuacion6 = Neg (Neg (Valor 2)) 

simplificar :: ExpA -> ExpA
simplificar (Valor n1) = Valor n1
simplificar (Sum n1 n2) = simplificarSuma (simplificar n1) (simplificar n2)
simplificar (Prod n1 n2) = simplificarProd (simplificar n1) (simplificar n2)
simplificar (Neg n1) = simplificarNeg (simplificar n1)

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Valor 0) n = n 
simplificarSuma n (Valor 0) = n 
simplificarSuma n1 n2 = Sum n1 n2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) n = (Valor 0)
simplificarProd n (Valor 0) = (Valor 0)
simplificarProd (Valor 1) n = n
simplificarProd n (Valor 1) = n
simplificarProd n1 n2 = (Prod n1 n2)

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg n) = n
simplificarNeg n = Neg n 