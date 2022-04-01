-- 1

succesor :: Int -> Int
succesor n = n + 1

sumar :: Int -> Int -> Int
sumar n1 n2 = n1 + n2

divisionYResto :: Int -> Int -> (Int , Int)
divisionYResto n1 n2 = (div n1 n2, mod n1 n2)

maxDelPar :: (Int, Int) -> Int
maxDelPar t = max (fst t) (snd t)


-- 2

data Dir = Norte | Sur | Este | Oeste deriving Show

opuesto :: Dir -> Dir
opuesto Norte   = Sur
opuesto Sur     = Norte
opuesto Este    = Oeste
opuesto Oeste   = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur     = True
iguales Este Este   = True
iguales Oeste Oeste = True
iguales _ _         = False

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
siguiente Oeste = error "No hay siguiente"


-- 3

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

primerYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primerYUltimoDia = (Domingo, Sabado)

empiezaConM :: DiaDeSemana ->Bool
empiezaConM Martes      = True
empiezaConM Miercoles   = True
empiezaConM _           = False

vieneDespues ::  DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = esMayor (valorDeDia d1) (valorDeDia d2)

esMayor :: Int -> Int -> Bool
esMayor n1 n2 = n1 > n2

valorDeDia :: DiaDeSemana -> Int
valorDeDia Lunes        = 1
valorDeDia Martes       = 2
valorDeDia Miercoles    = 3
valorDeDia Jueves       = 4
valorDeDia Viernes      = 5
valorDeDia Sabado       = 6
valorDeDia Domingo      = 7

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio dia = esPrimerOUltimoDia dia

esPrimerOUltimoDia :: DiaDeSemana -> Bool
esPrimerOUltimoDia d = esMismoDia d (fst primerYUltimoDia) || esMismoDia d (snd primerYUltimoDia)

esMismoDia :: DiaDeSemana -> DiaDeSemana -> Bool
esMismoDia Lunes        Lunes   = True
esMismoDia Martes       Martes  = True
esMismoDia Miercoles    Miercoles = True
esMismoDia Jueves       Jueves  = True
esMismoDia Viernes      Viernes = True
esMismoDia Sabado       Sabado  = True
esMismoDia Domingo      Domingo = True
esMismoDia _            _       = False


-- 4
--`

negar :: Bool -> Bool
negar True  = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True False  = False
implica _ _         = True

and :: Bool -> Bool -> Bool
and False _    = False
and b1   b2  = b2

or :: Bool -> Bool -> Bool
or True _  = True
or b1   b2  = b2


-- Registros

-- Nombre = String -> POKES
type Edad = Int
data Persona = P Nombre Edad

juan = P "Juan" 3

nombre :: Persona -> Nombre
nombre (P s i) = s

edad :: Persona -> Edad
edad (P s i) = i

crecer :: Persona -> Persona
crecer (P s i) = (P s (i+1))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre s1 (P s i) = (P s1 i)

esMayoQueLaOtra :: Persona -> Persona -> Bool
esMayoQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if (esMayoQueLaOtra p1 p2)
                        then p1
                        else p2


--Pokes
type Energia = Int
type Nombre = String

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = Pokemon TipoDePokemon Energia
data Entrenador = En Nombre Pokemon Pokemon

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (Pokemon t e) = t

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esMejorTipo (tipoPokemon p1) (tipoPokemon p2)

esMejorTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMejorTipo Agua    Fuego   = True
esMejorTipo Fuego   Planta  = True
esMejorTipo Planta  Agua    = True
esMejorTipo _ _             = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (En n p1 p2) = unoSi (esMismoTipo tipo (tipoPokemon p1))
                                                + unoSi (esMismoTipo tipo (tipoPokemon p2))
esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua    Agua    = True
esMismoTipo Fuego   Fuego   = True
esMismoTipo Planta  Planta  = True
esMismoTipo _ _             = False

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1,e2) = pokemonesDe e1 ++ pokemonesDe e2 

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (En n p1 p2) = (p1:p2:[])

-- Func. polimorficas
loMismo :: a -> a
loMismo a = a

siempreSiete :: a -> Int
siempreSiete _ = 7

swap :: (a,b) -> (b,a)
swap t = (snd t , fst t)


-- PM sobre listas

estaVacia ::[a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero (x:xs) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (x:xs) = xs

splitHead :: [a] -> (a,[a])
splitHead ls = (elPrimero ls, sinElPrimero ls)