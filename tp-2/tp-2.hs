-- 1) 1 a 15

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x : xs) = x + sumatoria xs 

longitud :: [a] -> Int
longitud [] = 0
longitud (x : xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x : xs) = x + 1 : sucesores xs

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x : xs) = x && conjuncion xs

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x : xs) = x || disyuncion xs

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x : xs) = x ++ aplanar xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x : xs) = e == x || (pertenece e xs)

apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x : xs) = (if e == x then 1 else 0) + apariciones e xs

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (x : xs) = if x < n then x : losMenoresA n xs else losMenoresA n xs

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (xs : xss) = if (longitud xs > n) then xs : lasDeLongitudMayorA n xss else lasDeLongitudMayorA n xss

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] a = [a]
agregarAlFinal (x : xs) a = x : agregarAlFinal xs a

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar  (x : xs) ys = x : agregar xs ys

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] l = l
zipMaximos l [] = l
zipMaximos (x : xs) (y : ys) = if x >= y then (x : zipMaximos xs ys) else (y : zipMaximos xs ys)

elMinimo :: Ord a => [a] -> a
elMinimo [] = error "la lista no debe ser vacia"
elMinimo (x : []) = x
elMinimo (x : xs) = if x < elMinimo xs then x else (elMinimo xs)

--2) 1 a 5

factorial :: Int -> Int
factorial 0 = 1
factorial n = if (n < 0) then error "n no puede ser negativo" else (n * factorial (n-1))

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 1 = [1]
cuentaRegresiva n = if n < 1 then [] else n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : (repetir (n-1) e)

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 l = []
losPrimeros n [] = []
losPrimeros n (x : xs) = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 l = l
sinLosPrimeros n [] = []
sinLosPrimeros n (x : xs) = sinLosPrimeros (n-1) xs

--3) 1

data Persona = P String Int deriving Show

-- para testear
yo:: Persona
yo = P "Orne" 21
yo2 = P "Victoria" 18

edad :: Persona -> Int
edad (P _ e) = e

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (x : xs) = if (edad x > n) then x : mayoresA n xs else mayoresA n xs 

promedioEdad :: [Persona] -> Int
promedioEdad [] = error "la lista no puede ser vacia"
promedioEdad ps = (div (sumatoriaDeEdades ps) (longitud ps))

sumatoriaDeEdades :: [Persona] -> Int
sumatoriaDeEdades [] = 0
sumatoriaDeEdades (x : xs) = edad x + sumatoriaDeEdades xs

elMasViejo :: [Persona] -> Persona
elMasViejo [] = error "la lista debe tener al menos una persona"
elMasViejo (p:[]) = p
elMasViejo (p : ps) = if (esMasViejo p (elMasViejo ps)) then p else elMasViejo ps

-- subtarea
esMasViejo :: Persona -> Persona -> Bool
esMasViejo p1 p2 = edad p1 > edad p2


-- 3) 2

-- para testear
aguita = ConsPokemon Agua 50
fueguito = ConsPokemon Fuego 70
plantita = ConsPokemon Planta 60
entrenador1 = ConsEntrenador "Orne" [aguita , fueguito , plantita]
entrenador2 = ConsEntrenador "Pepe" [fueguito , plantita]
entrenador3 = ConsEntrenador "Peppa" [aguita]
entrenador4 = ConsEntrenador "Leon" [fueguito]

data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int deriving Show
data Entrenador = ConsEntrenador String [Pokemon] deriving Show

tipoP :: Pokemon -> TipoDePokemon
tipoP (ConsPokemon t _) = t

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ ps) = longitud ps

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ []) = 0
cantPokemonDe t (ConsEntrenador _ ps) = pokemonsDeTipo t ps

--subtareas

pokemonsDeTipo :: TipoDePokemon -> [Pokemon] -> Int
pokemonsDeTipo t [] = 0 
pokemonsDeTipo t (x:xs) = unoSiEsDeTipo t x + pokemonsDeTipo t xs

unoSiEsDeTipo :: TipoDePokemon -> Pokemon -> Int
unoSiEsDeTipo t (ConsPokemon t1 _) = if esMismoTipo t t1 then 1 else 0

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Fuego Fuego = True
esMismoTipo Agua Agua = True
esMismoTipo Planta Planta = True
esMismoTipo t t1 = False

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = cuantosLeGanan t (pokemons e1) (pokemons e2)

--subtareas 
pokemons :: Entrenador -> [Pokemon]
pokemons (ConsEntrenador _ p) = p

cuantosLeGanan :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cuantosLeGanan t [] pss = 0
cuantosLeGanan t (p : ps) pss = unoSiCumpleTodo t p pss + cuantosLeGanan t ps pss

unoSiCumpleTodo :: TipoDePokemon -> Pokemon -> [Pokemon] -> Int
unoSiCumpleTodo t p ps = if (esDeTipo t p && leGanaATodos p ps) then 1 else 0

esDeTipo :: TipoDePokemon -> Pokemon -> Bool
esDeTipo t (ConsPokemon t1 _) = esMismoTipo t t1

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos p (p1:[]) = True
leGanaATodos p (p1:ps) = esMejorPokemon p p1 && leGanaATodos p ps

esMejorPokemon :: Pokemon -> Pokemon -> Bool
esMejorPokemon (ConsPokemon a _) (ConsPokemon b _) = esMejorTipoQue a b

esMejorTipoQue :: TipoDePokemon -> TipoDePokemon -> Bool
esMejorTipoQue Agua t1 = esTipoFuego t1
esMejorTipoQue Fuego t2 = esTipoPlanta t2
esMejorTipoQue Planta t3 = esTipoAgua t3

esTipoFuego :: TipoDePokemon -> Bool
esTipoFuego Fuego = True
esTipoFuego _ = False

esTipoPlanta :: TipoDePokemon -> Bool
esTipoPlanta Planta = True
esTipoPlanta _ = False

esTipoAgua :: TipoDePokemon -> Bool
esTipoAgua Agua = True
esTipoAgua _ = False

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ps) = hayUnPokemonDe Fuego ps && hayUnPokemonDe Planta ps && hayUnPokemonDe Agua ps

hayUnPokemonDe :: TipoDePokemon -> [Pokemon] -> Bool
hayUnPokemonDe t [] = False
hayUnPokemonDe t (p : ps) = (esMismoTipo t (tipoP p)) || (hayUnPokemonDe t ps)

-- 3) 3-
-- El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
-- de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
-- una lista de personas con diferente rol. La definición es la siguiente:

data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

--para testear
empresa1 = ConsEmpresa [trabajador1 , trabajador2 , trabajador3, trabajador2, trabajador4]
empresa2 = ConsEmpresa [trabajador1, trabajador1, trabajador1, trabajador1]
empresa3 = ConsEmpresa [trabajador4, trabajador4]
empresa4 = ConsEmpresa [] 

trabajador1 = Developer Junior proyecto1
trabajador2 = Developer Senior proyecto2
trabajador3 = Management SemiSenior proyecto1
trabajador4 = Management Senior proyecto3

proyecto1 = ConsProyecto "X"
proyecto2 = ConsProyecto "Y"
proyecto3 = ConsProyecto "Z"

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa []) = []
proyectos (ConsEmpresa rs) = proyectosSinRepetidos (listaDeProyectos rs)

listaDeProyectos :: [Rol] -> [Proyecto]
listaDeProyectos [] = []
listaDeProyectos (r : rs) = proyectoDeRol r : listaDeProyectos rs

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer _ p) = p
proyectoDeRol (Management _ p) = p

proyectosSinRepetidos :: [Proyecto] -> [Proyecto]
proyectosSinRepetidos [] = []
proyectosSinRepetidos ps = listaSinRepetidos [] ps

sonElMismoProyecto :: Proyecto -> Proyecto -> Bool
sonElMismoProyecto p1 p2 = (nombreDelProyecto p1 == nombreDelProyecto p2)

nombreDelProyecto :: Proyecto -> String
nombreDelProyecto (ConsProyecto s) = s

listaSinRepetidos :: [Proyecto] -> [Proyecto] -> [Proyecto]
listaSinRepetidos _ [] = []
listaSinRepetidos nombresVistos (p : ps) = if estaEnLaLista p nombresVistos
then listaSinRepetidos nombresVistos ps
else p : listaSinRepetidos (p : nombresVistos) ps

estaEnLaLista :: Proyecto -> [Proyecto] -> Bool
estaEnLaLista _ [] = False
estaEnLaLista s (n : ns) = sonElMismoProyecto s n || estaEnLaLista s ns

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior es ps = cantDeEmpleadosQueEstanEnAlgunProyecto (seniors es) ps

--subtareas
seniors :: Empresa -> [Rol]
seniors (ConsEmpresa rs) = listaDeSeniors rs

listaDeSeniors :: [Rol] -> [Rol]
listaDeSeniors [] = []
listaDeSeniors (r : rs) = if esSenior r then r : listaDeSeniors rs else listaDeSeniors rs

-- data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
esSenior :: Rol -> Bool
esSenior (Developer r _) = esRolSenior r
esSenior (Management r _) = esRolSenior r

esRolSenior :: Seniority -> Bool
esRolSenior Senior = True
esRolSenior _ = False

cantDeEmpleadosQueEstanEnAlgunProyecto :: [Rol] -> [Proyecto] -> Int
cantDeEmpleadosQueEstanEnAlgunProyecto [] ps = 0
cantDeEmpleadosQueEstanEnAlgunProyecto (r : rs) ps = unoSiEstaEnAlgunProyecto r ps + cantDeEmpleadosQueEstanEnAlgunProyecto rs ps 

unoSiEstaEnAlgunProyecto :: Rol -> [Proyecto] -> Int
unoSiEstaEnAlgunProyecto r [] = 0
unoSiEstaEnAlgunProyecto r (p : ps) = if perteneceAProyecto r p then 1 else unoSiEstaEnAlgunProyecto r ps 

perteneceAProyecto :: Rol -> Proyecto -> Bool
perteneceAProyecto (Developer _ p) p1 = sonElMismoProyecto p p1
perteneceAProyecto (Management _ p) p1 = sonElMismoProyecto p p1

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = cantDeEmpleadosQueEstanEnAlgunProyecto rs ps

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
-- devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
-- cantidad de personas involucradas.
asignadosPorProyecto (ConsEmpresa []) = []
asignadosPorProyecto (ConsEmpresa rs) = paresDeProyectos rs

paresDeProyectos :: [Rol] -> [(Proyecto, Int)]
paresDeProyectos [] = []
paresDeProyectos (r : rs) = agregarProyecto r (paresDeProyectos rs)

agregarProyecto :: Rol -> [(Proyecto, Int)] -> [(Proyecto, Int)]
agregarProyecto r [] = [(proyectoDeRol r,1)]
agregarProyecto r ((p,n) : xs) = if perteneceAProyecto r p then (p , n+1): xs else (p,n) : agregarProyecto r xs