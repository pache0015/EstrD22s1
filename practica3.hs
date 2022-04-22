unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia

-- nroBolitas :: Color -> Celda -> Int
-- nroBolitas _ CeldaVacia     = 0
-- nroBolitas c1 (Bolita c2 cel) = unoSi(mismoColor c1 c2) + nroBolitas c1 cel

-- poner :: Color -> Celda -> Celda
-- poner c1 CeldaVacia      = (Bolita c1 CeldaVacia)
-- poner c1 (Bolita c2 cel) = (Bolita c1 (Bolita c2 cel))

-- sacar :: Color -> Celda -> Celda
-- sacar col1 CeldaVacia = CeldaVacia
-- sacar col1 (Bolita col2 cel) = if (esMismoColor col1 col2)
--                                 then cel
--                                 else (Bolita col2 cel)

ponerN :: Int -> Color -> Celda -> Celda
ponerN n col1 CeldaVacia = (Bolita col1 CeldaVacia)
ponerN n col1 (Bolita col2 cel) = if n >= 1
                                    then (Bolita col1 (ponerN (n-1) col2 cel))
                                    else (Bolita col2 cel)


data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada cam) = hayTesoro cam
hayTesoro (Cofre objs cam) = (tieneTesoro objs) || hayTesoro cam

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (x:xs) = esTesoro x || tieneTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Cofre objs cam) = if tieneTesoro objs
                                        then 0
                                        else 1 + pasosHastaTesoro cam
pasosHastaTesoro (Nada cam) = 1 + pasosHastaTesoro cam

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 camino = pocisionConTesoro camino
hayTesoroEn n camino = hayTesoroEn (n-1) camino

pocisionConTesoro :: Camino -> Bool
pocisionConTesoro (Cofre objs cam) = tieneTesoro objs
pocisionConTesoro _ = False


alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = n <= cantTesorosEnCamino camino

cantTesorosEnCamino:: Camino -> Int
cantTesorosEnCamino Fin = 0
cantTesorosEnCamino (Cofre objs cam) = if tieneTesoro objs
                                        then 1 + cantTesorosEnCamino cam
                                        else cantTesorosEnCamino cam
cantTesorosEnCamino (Nada cam) = cantTesorosEnCamino cam

--Desafio
cantTesorosEntre :: Int -> Int -> Camino -> Int
--Crei necesario hacer un doble pm porque solo necesito hacer uso de cantTesorosHasta cuando el primer n es cero.
cantTesorosEntre 0 n cam = cantTesorosHasta n cam
cantTesorosEntre n _ Fin = 0
cantTesorosEntre n1 n2 (Cofre objs cam) = cantTesorosEntre (n1-1) n2 cam
cantTesorosEntre n1 n2 (Nada cam) = cantTesorosEntre (n1-1) n2 cam

cantTesorosHasta :: Int -> Camino -> Int
cantTesorosHasta n Fin = 0
cantTesorosHasta n (Cofre objs cam) = if n >= 0
                                        then unoSi(tieneTesoro objs) + cantTesorosHasta (n-1) cam
                                        else cantTesorosHasta (n-1) cam
cantTesorosHasta n (Nada cam) = cantTesorosHasta (n-1) cam


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n ti td) = n + sumarT ti + sumarT td

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT e ti td) = 1 + sizeT ti + sizeT td

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n ti td) = NodeT (n*2) (mapDobleT ti) (mapDobleT td)

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT e1 (NodeT e2 ti td) = e1 == e2 || perteneceT e1 ti || perteneceT e1 td

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT e1 (NodeT e2 ti td) = unoSi (e1 == e2) + aparicionesT e1 ti + aparicionesT e2 td

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT e ti td) = e : leaves ti ++ leaves td

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT e ti td) = 1 + max (heightT ti) (heightT td)

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT e ti td) = NodeT e (mirrorT td) (mirrorT ti)

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT e ti td) = toList ti ++ [e] ++ toList td

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT e ti td) = [e]
levelN n (NodeT e ti td) = levelN (n-1) ti ++ levelN (n-1) td

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT e ti td) =  [e] : posicionAPosicion (listPerLevel ti) (listPerLevel td)
-- Lo que quiero obtener: [[1],[2,3],[4,5,6]]

posicionAPosicion :: [[a]] -> [[a]] -> [[a]]
posicionAPosicion [] [] = []
posicionAPosicion [] (y:ys) = y : posicionAPosicion [] ys
posicionAPosicion (x:xs) [] = x : posicionAPosicion xs []
posicionAPosicion (x:xs) (y:ys) = [x,y] ++ posicionAPosicion xs ys 


--                1
--            /      \
--          2         3
--        /   \      /
--       4    5     6

tree = NodeT 1
                (NodeT 2
                    (NodeT 4 EmptyT EmptyT)
                    (NodeT 5 EmptyT EmptyT))
                (NodeT 3
                    (NodeT 6 EmptyT EmptyT)
                    EmptyT)

tree' = NodeT 1
                (NodeT 2
                    (NodeT 4 EmptyT EmptyT)
                    (NodeT 5 EmptyT 
                                    (NodeT 7 EmptyT EmptyT)))
                (NodeT 3
                    (NodeT 6 EmptyT EmptyT)
                    EmptyT)

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT e ti td) = e : ramaMasLarga (laMasLarga ti td)

laMasLarga :: Tree a -> Tree a -> Tree a
laMasLarga ti td = if heightT ti > heightT td
                    then ti
                    else td

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT e ti td) = agregaE e (todosLosCaminos ti) ++ agregaE e (todosLosCaminos td)
-- Quiero que de: [[1,2,4],[1,2,5],[1,3,6]]

agregaE :: a -> [[a]] -> [[a]]
agregaE _ [] = []
agregaE e (x:xs) = [e:x] ++ agregaE e xs

--                1
--            /      \
--          2         3
--        /   \      /
--       4    5     6





data ExpA = Valor Int
            | Sum ExpA ExpA
            | Prod ExpA ExpA
            | Neg ExpA

eval :: ExpA -> Int
eval (Valor n)      = n
eval (Sum   e1 e2)  = eval e1 + eval e2
eval (Prod  e1 e2)  = eval e1 * eval e2
eval (Neg   e1)     = - (eval e1)

exp  = Sum (Valor 2) (Valor 3)
exp2 = Prod (Valor 2) (Valor 3)
exp3 = Neg (Valor 2)

-- simplificar :: ExpA -> ExpA
-- simplificar (Valor n)      = Valor n
-- simplificar (Sum   e1 e2)  = simplificarSum e1 e2
-- simplificar (Prod  e1 e2)  = simplificarSProd e1 e2
-- simplificar (Neg   e1)     = simplificarNeg e1



-- Dudas:
--  - simplificar

-- Para pasar codigo en dc: ```