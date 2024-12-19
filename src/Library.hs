{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Nave = UnaNave{
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poder :: Poder
} deriving (Show, Eq)

type Poder = Nave -> Nave

tieFighter, xWing, darthVader, millenniumFalcon, uri :: Nave

tieFighter = UnaNave 200 100 50 turbo
xWing = UnaNave 300 150 100 repaEmergencia
darthVader = UnaNave 500 300 200 superTurbo
millenniumFalcon = UnaNave 1000 500 50 (repaEmergencia . disminuirEscudos (-100))
uri = UnaNave 600 400 40 giroPro

turbo :: Poder
turbo = disminuirAtaque (-25)

repaEmergencia :: Poder
repaEmergencia = restarDurabilidad (-50) . disminuirAtaque 30

restarDurabilidad :: Number -> Nave -> Nave
restarDurabilidad n nave = nave{durabilidad = restaNoNegativa (durabilidad nave) n}

restaNoNegativa :: Number -> Number -> Number
restaNoNegativa numero otroNum = max 0 (numero-otroNum)

disminuirAtaque :: Number -> Nave -> Nave
disminuirAtaque n nave = nave{ataque = restaNoNegativa (ataque nave) n}

superTurbo :: Poder
superTurbo = restarDurabilidad 45 . turbo . turbo . turbo

disminuirEscudos :: Number -> Nave -> Nave
disminuirEscudos n nave = nave{escudo = restaNoNegativa (escudo nave) n}

giroPro :: Poder
giroPro nave = (disminuirEscudos (ataque nave / 2) . repaEmergencia) nave

--2

type Flota = [Nave]

durabilidadTotal :: Flota -> Number
durabilidadTotal flota = sum (map durabilidad flota)

--3

resultadoAtaque :: Nave -> Nave -> Nave
resultadoAtaque naveAtacada atacante = atacar(activarPoder atacante) (activarPoder naveAtacada)

atacar :: Nave -> Nave -> Nave
atacar atacante atacada = restarDurabilidad(danio atacante atacada) atacada

danio :: Nave -> Nave -> Number
danio atacante atacada = restaNoNegativa (ataque atacante) (escudo atacada)

activarPoder :: Nave -> Nave
activarPoder nave = poder nave nave

--4

estaFueraDeCombate :: Nave -> Bool
estaFueraDeCombate nave = durabilidad nave == 0

--5

type Estrategia = Nave -> Bool

realizarMisionSorpresa :: Estrategia -> Nave -> Flota -> Flota
realizarMisionSorpresa estrategia nave  = mapSelectivo (atacar nave) estrategia 

mapSelectivo :: Show a => (a -> a) -> (a -> Bool) -> [a] -> [a]
mapSelectivo cambio condicion lista = map cambio (filter condicion lista) ++ filter(not . condicion)lista

navesDebiles :: Estrategia
navesDebiles nave = escudo nave < 200

navesConPeligrosidad :: Number -> Estrategia
navesConPeligrosidad n nave = ataque nave > n

navesQueQuedanFueraDeCombate :: Nave -> Estrategia
navesQueQuedanFueraDeCombate atacante = estaFueraDeCombate . atacar atacante

--naves con ataque mayor a 80 luego de activar su poder
navesPicadas :: Estrategia
navesPicadas = navesConPeligrosidad 80 . activarPoder 

--6

minimizaDurabilidadTotal :: Nave -> Estrategia -> Estrategia -> Flota -> Estrategia
minimizaDurabilidadTotal nave est1 est2 flota
    |durabilidadTotal (realizarMisionSorpresa est1 nave flota) < durabilidadTotal (realizarMisionSorpresa est2 nave flota) = est1
    |otherwise = est2

atacarConMejorEstrategia :: Nave -> Estrategia -> Estrategia -> Flota -> Flota
atacarConMejorEstrategia nave est1 est2 flota = realizarMisionSorpresa (minimizaDurabilidadTotal nave est1 est2 flota) nave flota

--7

flotaInfinita :: Flota
flotaInfinita = millenniumFalcon:flotaInfinita

--No es posible determinar durabilidad total
--Nada ya que no puede retornar una flota infinita