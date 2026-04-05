{-2. Números enteros
1. Defina las siguientes funciones:
a) sucesor :: Int -> Int
Dado un número devuelve su sucesor
b) sumar :: Int -> Int -> Int
Dados dos números devuelve su suma utilizando la operación +.
c) divisionYResto :: Int -> Int -> (Int, Int)
Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell.
d) maxDelPar :: (Int,Int) -> Int
Dado un par de números devuelve el mayor de estos.
-}

sucesor :: Int -> Int
sucesor n = n+1

sumar :: Int -> Int -> Int
sumar n m = n+m

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

maxDelPar :: (Int,Int) -> Int
maxDelPar (n,m) = if n < m then m else n

{-
2. De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))

° sumar (sucesor (5)) (maxDelPar (divisionYResto 8 2))
° sucesor (sumar (maxDelPar (divisionYResto 3 2)) 8)
° maxDelPar (divisionYResto (sumar 95 5) (sucesor 9))
° sumar (maxDelPar (divisionYResto 10 2)) (sucesor (4))
-}

{-3. Tipos enumerativos
1. Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar
las siguientes funciones:
a) opuesto :: Dir -> Dir
Dada una dirección devuelve su opuesta.
b) iguales :: Dir -> Dir -> Bool
Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
c) siguiente :: Dir -> Dir
Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. ¾Posee una precondición esta función? ¾Es una función
total o parcial? Por qué?-}

data Dir = Norte | Sur | Este | Oeste

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "no existe la siguiente dirección a Oeste"
-- es parcial ya que solo se definen resultados para un subconjunto del dominio, 
-- es decir que arroja error si se recibe la entrada "oeste"

{-2. Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones:
a) primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
Devuelve un par donde la primera componente es el primer día de la semana, y la
segunda componente es el último día de la semana. Considerar definir subtareas útiles
que puedan servir después.
b) empiezaConM :: DiaDeSemana -> Bool
Dado un día de la semana indica si comienza con la letra M.
c) vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).
Ejemplo: vieneDespues Jueves Lunes = True
d) estaEnElMedio :: DiaDeSemana -> Bool
Dado un día de la semana indica si no es ni el primer ni el ultimo dia.-}

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = diaANumero dia1 > diaANumero dia2 

-- pasa de dia a numero segun la posicion en la semana
diaANumero :: DiaDeSemana -> Int
diaANumero Lunes = 1
diaANumero Martes = 2
diaANumero Miercoles = 3
diaANumero Jueves = 4
diaANumero Viernes = 5 
diaANumero Sabado = 6
diaANumero Domingo = 7

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

{-3. Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina
las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
ya definidas en Haskell):
a) negar :: Bool -> Bool
Dado un booleano, si es True devuelve False, y si es False devuelve True.
En Haskell ya está definida como not.
b) implica :: Bool -> Bool -> Bool
Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True.
Esta función NO debe realizar doble pattern matching.
Nota: no viene implementada en Haskell.
c) yTambien :: Bool -> Bool -> Bool
Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como \&\&.
d) oBien :: Bool -> Bool -> Bool
Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como ||.-}

negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True b = b 
implica _ _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True b = b
yTambien False _ = False

oBien :: Bool -> Bool -> Bool
oBien False b = b
oBien True _ = True

{-4. Registros
1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:
nombre :: Persona -> String
Devuelve el nombre de una persona
edad :: Persona -> Int
Devuelve la edad de una persona
crecer :: Persona -> Persona
Aumenta en uno la edad de la persona.
cambioDeNombre :: String -> Persona -> Persona
Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
nuevo nombre.
esMayorQueLaOtra :: Persona -> Persona -> Bool
Dadas dos personas indica si la primera es mayor que la segunda.
laQueEsMayor :: Persona -> Persona -> Persona
Dadas dos personas devuelve a la persona que sea mayor.
2. Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego definir las
siguientes funciones:
superaA :: Pokemon -> Pokemon -> Bool
Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
Dado un par de entrenadores, devuelve a sus Pokémon en una lista.-}

-- 4) 1

data Persona = P String Int deriving Show

nombre :: Persona -> String
nombre (P n _) = n 

edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P nombre edad) = (P nombre (edad+1))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoNombre (P nombre edad) = (P nuevoNombre edad)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1  > edad p2 

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = 
    if edad p1 > edad p2 then p1 else p2

--4) 2

data Pokemon = Po TipoDePokemon Int deriving Show
data Entrenador = En String Pokemon Pokemon deriving Show
data TipoDePokemon = Agua | Fuego | Planta deriving Show

superaA :: Pokemon -> Pokemon -> Bool
superaA (Po a _) (Po b _) = esMejorTipoQue a b

--subtareas 
-- dados dos tipos de pokemons indica si el primer tipo es mejor que el segundo
esMejorTipoQue :: TipoDePokemon -> TipoDePokemon -> Bool
esMejorTipoQue Agua t1 = esTipoFuego t1
esMejorTipoQue Fuego t2 = esTipoPlanta t2
esMejorTipoQue Planta t3 = esTipoAgua t3

--dado un tipo de pokemon indica si es del tipo pedido
esTipoFuego :: TipoDePokemon -> Bool
esTipoFuego Fuego = True
esTipoFuego _ = False

esTipoPlanta :: TipoDePokemon -> Bool
esTipoPlanta Planta = True
esTipoPlanta _ = False

esTipoAgua :: TipoDePokemon -> Bool
esTipoAgua Agua = True
esTipoAgua _ = False

-- para testear 
aguita = Po Agua 50
fueguito = Po Fuego 70
plantita = Po Planta 60
entrenador1 = En "Orne" aguita fueguito
entrenador2 = En "Pepe" fueguito plantita

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (En _ p1 p2) = unoSiEsDeTipo t p1 + unoSiEsDeTipo t p2

--subtareas
--dados un tipo de pokemon y un pokemon indica 1 si el pokemon es del tipo dado y 0 si no
unoSiEsDeTipo :: TipoDePokemon -> Pokemon -> Int
unoSiEsDeTipo t (Po t1 _) = if esMismoTipo t t1 then 1 else 0

--indica si dos tipos son el mismo
esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Fuego t1 = esTipoFuego t1
esMismoTipo Agua t2 = esTipoAgua t2
esMismoTipo Planta t3 = esTipoPlanta t3

-- Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = listaDePokemons e1 ++ listaDePokemons e2

listaDePokemons :: Entrenador -> [Pokemon]
listaDePokemons (En _ p1 p2) = p1 : p2 : []

{-5. Funciones polimórficas
1. Defina las siguientes funciones polimórficas:
a) loMismo :: a -> a
Dado un elemento de algún tipo devuelve ese mismo elemento.
b) siempreSiete :: a -> Int
Dado un elemento de algún tipo devuelve el número 7.
c) swap :: (a,b) -> (b, a)
Dadas una tupla, invierte sus componentes.
Por qué existen dos variables de tipo diferentes?
2. Responda la siguiente pregunta: ¾Por qué estas funciones son polimórficas? -}

loMismo :: a -> a
loMismo a = a

siempreSiete :: a -> Int
siempreSiete a = 7

swap :: (a,b) -> (b, a)
swap (a,b) = (b,a)

-- 5) 2 porque pueden aplicarse a elementos de cualquier tipo.


{-6. Pattern matching sobre listas
1. Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no
utilizar las funciones que ya vienen con Haskell):
2. estaVacia :: [a] -> Bool
Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
Definida en Haskell como null.
3. elPrimero :: [a] -> a
Dada una lista devuelve su primer elemento.
Definida en Haskell como head.
Nota: tener en cuenta que el constructor de listas es :
4. sinElPrimero :: [a] -> [a]
Dada una lista devuelve esa lista menos el primer elemento.
Definida en Haskell como tail.
Nota: tener en cuenta que el constructor de listas es :
5. splitHead :: [a] -> (a, [a])
Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
lista, y la segunda componente es esa lista pero sin el primero.
Nota: tener en cuenta que el constructor de listas es :
-}

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False 

elPrimero :: [a] -> a
elPrimero (x : _) = x
elPrimero [] = error "la lista esta vacia"

sinElPrimero :: [a] -> [a]
sinElPrimero (x : y) = y 
sinElPrimero [] = error "la lista esta vacia"

splitHead :: [a] -> (a, [a])
splitHead (x : y) = (x , y)
