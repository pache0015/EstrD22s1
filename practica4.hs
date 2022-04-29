data Pizza = Prepizza
                | Capa Ingrediente Pizza
data Ingrediente = Salsa
                    | Queso
                    | Jamon
                    | Aceitunas Int

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing pi) = 1 + cantidadDeCapas pi

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing pi) = if esJamon ing
                            then pi
                            else (Capa ing pi)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso pi = soloSalsaYQueso (ingredientes pi)

soloSalsaYQueso :: [Ingrediente] -> Bool
soloSalsaYQueso [] = False
soloSalsaYQueso (x:xs) = not (esOtraCosaQueSalsaOQueso x || soloSalsaYQueso xs)

esOtraCosaQueSalsaOQueso :: Ingrediente -> Bool
esOtraCosaQueSalsaOQueso Jamon = True
esOtraCosaQueSalsaOQueso (Aceitunas n) = True
esOtraCosaQueSalsaOQueso _ = False

ingredientes :: Pizza -> [Ingrediente]
ingredientes Prepizza = []
ingredientes (Capa i p) = i : ingredientes p 

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = if (esAceituna i)
                                    then (Capa (aceitunaPorDos i) p)
                                    else (Capa i p)

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas n) = True
esAceituna _ = False

aceitunaPorDos :: Ingrediente -> Ingrediente
aceitunaPorDos (Aceitunas n) = Aceitunas (n*2)

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) = (cantCapas x, x) : cantCapasPorPizza xs

cantCapas :: Pizza -> Int
cantCapas p = length (ingredientes p)


data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre
            | Bifurcacion Cofre Mapa Mapa

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) =
hayTesoro (Bifurcacion c mi md) = tieneTesoro( objsCofre c ) || hayTesoro mi || hayTesoro md

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] m = hayTesoroAca m
hayTesoroEn (x:xs) (Fin c) = False
hayTesoroEn (x:xs) (Bifurcacion c mi md) = hayTesoroEn xs (mapaPorDir x mi md)

mapaPorDir :: Dir -> Mapa -> Mapa -> Mapa
mapaPorDir Izq mi _ = mi
mapaPorDir Der _ md = md

hayTesoroAca :: Mapa -> Bool
hayTesoroAca (Fin c) = tieneTesoro (objsCofre c)
hayTesoroAca (Bifurcacion c mi md) = tieneTesoro (objsCofre c)

caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c mi md) = if tieneTesoro (objsCofre c)
                                        then []
                                            if hayTesoro mi
                                                then Izq : caminoAlTesoro mi
                                                else Der : caminoAlTesoro md
-- caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- tesorosPorNivel :: Mapa -> [[Objeto]]
-- todosLosCaminos :: Mapa -> [[Dir]]


data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)

-- sectores :: Nave -> [SectorId]
-- poderDePropulsion :: Nave -> Int
-- barriles :: Nave -> [Barril]
-- agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- sectoresAsignados :: Tripulante -> Nave -> [SectorId]
-- tripulantes :: Nave -> [Tripulante]



type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
                | Explorador Nombre [Territorio] Lobo Lobo
                | Cria Nombre
data Manada = M Lobo

-- buenaCaza :: Manada -> Bool
-- elAlfa :: Manada -> (Nombre, Int)
-- losQueExploraron :: Territorio -> Manada -> [Nombre]

-- exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
-- exploradoresPorTerritorio m = territoriosExploradosPorLobos (todosLosTerritorios m) m

-- todosLosTerritorios :: Manada -> [Territorio]
-- todosLosTerritorios (Cria n)                = []
-- todosLosTerritorios (Cazador n ps l1 l2 l3)   = todosLosTerritorios l1 ++ todosLosTerritorios l2 ++ todosLosTerritorios l3
-- todosLosTerritorios (Explorador n ts l1 l2) = agregarSiNoEsta ts (todosLosTerritorios l1 ++ todosLosTerritorios l2)

-- agregarSiNoEsta :: [Territorio] -> [Territorio] -> [Territorio]
-- agregarSiNoEsta ts1 [] = ts1
-- agregarSiNoEsta [] ts2 = ts2
-- agregarSiNoEsta ts1 (x:xs) = if (pertenece x ts1)
--                                 then ts1 ++ agregarSiNoEsta xs
--                                 else x:ts1 ++ agregarSiNoEsta

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []      = False
pertenece e (x:xs)  = (e == x) || pertenece e xs

-- territoriosExploradosPorLobos :: [Territorio] -> Manada -> [(Territorio, [Nombre])]
-- territoriosExploradosPorLobos []        m  = []
-- territoriosExploradosPorLobos (x:xs)    m  = ( x , losQueExploraron x m) : territoriosExploradosPorLobos xs m



-- superioresDelCazador :: Nombre -> Manada -> [Nombre]
-- superioresDelCazador n (Cria n2)                = []
-- superioresDelCazador n (Explorador n2 ts l1 l2) = superioresDelCazador n l1 ++ superioresDelCazador n l2
-- superioresDelCazador n (Cazador n2 ps l1 l2 l3) = if n == n2
--                                                     then []
--                                                     else n2 : porArriba n (superioresDelCazador n l1) (superioresDelCazador n l2) (superioresDelCazador n l3)

--No supe pensarla