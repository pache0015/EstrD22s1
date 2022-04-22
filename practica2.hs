sumatoria :: [Int] -> Int
sumatoria []        = 0
sumatoria (x:xs)    = x + sumatoria xs

longitud :: [a] -> Int
longitud []     = 0 
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores []        = []
sucesores (x:xs)    = (x + 1) : sucesores xs


--Preguntar por caso base
conjuncion :: [Bool] -> Bool
conjuncion []       = True
conjuncion (x:xs)   = x && conjuncion xs

--Preguntar por caso base
disyuncion :: [Bool] -> Bool
disyuncion []       = False
disyuncion (x:xs)   = x || disyuncion xs

aplanar :: [[a]] -> [a]
aplanar []      = []
aplanar (x:xs)  = x ++ aplanar xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []      = False
pertenece e (x:xs)  = (e == x) || pertenece e xs

apariciones :: Eq a => a -> [a] -> Int
apariciones _ []        = 0
apariciones e (x:xs)    = unoSi (e == x) + apariciones e xs

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []        = []
losMenoresA n (x:xs)    = if n > x
                            then x : losMenoresA n xs
                            else losMenoresA n xs

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []        = []
lasDeLongitudMayorA n (x:xs)    = if (length x) > n
                                    then x : lasDeLongitudMayorA n xs
                                    else lasDeLongitudMayorA n xs

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

concatenar :: [a] -> [a] -> [a]
concatenar [] ys         = ys
concatenar (x:xs) ys    = x : concatenar xs ys

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys = ys
zipMaximos xs [] = xs
zipMaximos(x:xs) (y:ys) = max x y : zipMaximos xs ys

elMinimo :: Ord a => [a] -> a
elMinimo [] = error ""
elMinimo (x:[]) = x
elMinimo (x:xs) = min x (elMinimo xs)


factorial :: Int -> Int
factorial 0 = 0
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n >= 1
                        then n : cuentaRegresiva (n-1)
                        else []

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n-1) e

losPrimeros :: Int -> [a] -> [a]
losPrimeros _ [] = []
losPrimeros 0 xs = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ [] = []
sinLosPrimeros 0 xs = xs
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs


type Edad = Int
type Nombre = String
data Persona = P Nombre Edad

edad :: Persona -> Edad
edad (P s i) = i

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (x:xs) = if edad x > n
                        then x : mayoresA n xs
                        else mayoresA n xs

promedioEdad :: [Persona] -> Int
promedioEdad xs = div (sumatoriaEdades xs) (length xs)

sumatoriaEdades :: [Persona] -> Int
sumatoriaEdades [] = 0
sumatoriaEdades (x:xs) = edad x + sumatoriaEdades xs

elMasViejo :: [Persona] -> Persona
elMasViejo [] = error ""
elMasViejo (x:[]) = x
elMasViejo (x:xs) = maxEdades x (elMasViejo xs)

maxEdades :: Persona -> Persona -> Persona
maxEdades p1 p2 = if edad p1 > edad p2
                    then p1
                    else p2


data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (ConsPokemon t e) = t

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esMejorTipo (tipoPokemon p1) (tipoPokemon p2)

esMejorTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMejorTipo Agua    Fuego   = True
esMejorTipo Fuego   Planta  = True
esMejorTipo Planta  Agua    = True
esMejorTipo _ _             = False

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador s ps) = length ps

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador s ps) = cantDeTipo t ps

cantDeTipo :: TipoDePokemon -> [Pokemon] -> Int
cantDeTipo _ [] = 0
cantDeTipo t (x:xs) = unoSi (esMismoTipo t (tipoPokemon x)) + cantDeTipo t xs

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua    Agua    = True
esMismoTipo Fuego   Fuego   = True
esMismoTipo Planta  Planta  = True
esMismoTipo _ _             = False

losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan t (ConsEntrenador s1 ps1) (ConsEntrenador s2 ps2) = cuantosGanan (losDelTipo t ps1) ps2

losDelTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
losDelTipo _ [] = []
losDelTipo t (x:xs) = if esMismoTipo t (tipoPokemon x)
                        then x : losDelTipo t xs
                        else losDelTipo t xs

cuantosGanan :: [Pokemon] -> [Pokemon] -> Int
cuantosGanan [] _ = 0
cuantosGanan (x:xs) ps = unoSi (ganaATodos x ps) + cuantosGanan xs ps

ganaATodos :: Pokemon -> [Pokemon] -> Bool
ganaATodos _ [] = True
ganaATodos p (x:xs) = superaA p x && ganaATodos p xs


data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto s) = s

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = proyectosDeRoles rs

proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles [] = []
proyectosDeRoles (x:xs) = proyecto x : proyectosDeRoles xs

proyecto :: Rol -> Proyecto
proyecto (Developer s p) = p 
proyecto (Management s p) = p

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = cantProyectosDeRolEnProyecto (proyectosDeRoles (soloSenior rs)) ps

soloSenior :: [Rol] -> [Rol]
soloSenior [] = []
soloSenior (x:xs) = ienior x
                        then x : soloSenior xs
                        else soloSenior xs 

esSenior :: Rol -> Bool
esSenior r = esSenioritySenior (seniority r)

seniority :: Rol -> Seniority
seniority (Developer s _) = s
seniority (Management s _) = s

esSenioritySenior :: Seniority -> Bool
esSenioritySenior Senior = True
esSenioritySenior _ = False

cantProyectosDeRolEnProyecto :: [Proyecto] -> [Proyecto] -> Int
cantProyectosDeRolEnProyecto [] _ = 0
cantProyectosDeRolEnProyecto (x:xs) ps = unoSi (proyectoPerteneceAProyectos x ps) + cantProyectosDeRolEnProyecto xs ps

proyectoPerteneceAProyectos :: Proyecto -> [Proyecto] -> Bool
proyectoPerteneceAProyectos _ [] = False
proyectoPerteneceAProyectos p (x:xs) = mismoProyecto p x || proyectoPerteneceAProyectos p xs

mismoProyecto :: Proyecto -> Proyecto -> Bool
mismoProyecto p1 p2 = nombreProyecto p1 == nombreProyecto p2

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = cantProyectosDeRolEnProyecto (proyectosDeRoles rs) ps

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = proyectosYCantRoles (todosLosProyectos rs)

todosLosProyectos :: [Rol] -> [Proyecto]
todosLosProyectos [] = []
todosLosProyectos (x:xs) = proyecto x : todosLosProyectos xs

proyectosYCantRoles :: [Proyecto] -> [(Proyecto , Int)]
proyectosYCantRoles []      = []
proyectosYCantRoles (x:xs)  = ( x , (cantApariciones  x xs)) : proyectosYCantRoles (quitarApariciones x xs)

cantApariciones :: Proyecto -> [(Proyecto , Int)] -> Int
cantApariciones p [] = 0
cantApariciones p (x:xs) = unoSi (mismoProyecto p (fst x)) + cantApariciones p xs

quitarApariciones :: Proyecto -> [(Proyecto , Int)] -> [(Proyecto , Int)]
quitarApariciones p [] = []
quitarApariciones p (x:xs) = if mismoProyecto p (fst x)
                                then quitarApariciones p xs
                                else x : quitarApariciones p xs
