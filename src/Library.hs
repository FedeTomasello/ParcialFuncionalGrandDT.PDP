module Library where
import PdePreludat
import GHC.RTS.Flags (ParFlags)

-- Declaraciones---

type Partido = (Number, Number)

minutos :: Partido -> Number
minutos = fst

goles :: Partido -> Number
goles = snd

data Posicion = Arquero | Defensor | Volante | Delantero deriving (Show,Eq)

data Jugador = UnJugador {

nombre :: String,
velocidad :: Number,
puesto :: Posicion,
habilidad :: Number,
partidos :: [Partido]


                        } deriving (Show,Eq) 

-- Mis Jugadores --

pupiSalmeron :: Jugador
pupiSalmeron = UnJugador {

nombre = "Pupi Salmeron",
velocidad = 33,
puesto = Delantero,
habilidad = 78,
partidos = [(90,3),(90,2),(45,1)]

}

garrafaSanchez :: Jugador
garrafaSanchez = UnJugador {

nombre = "Garrafa Sanchez",
velocidad = 88,
puesto = Volante,
habilidad = 99,
partidos = [(90,0),(90,1),(90,1)]

}

satanasPaez :: Jugador
satanasPaez = UnJugador {

nombre = "Satanas Paez",
velocidad = 11,
puesto = Defensor,
habilidad = 10,
partidos = [(90,0),(90,0),(90,0)]

}

caniete :: Jugador
caniete = UnJugador {

nombre = "Adolfo Caniete",
velocidad = 77,
puesto = Volante,
habilidad = 78,
partidos = [(0,0),(0,0),(45,1)]

}

bigliaBurro :: Jugador
bigliaBurro = UnJugador {

nombre = "Lucas Biglia",
velocidad = 10,
puesto = Volante,
habilidad = 1,
partidos = [(0,0),(0,0),(0,0)]

}
laSelesio :: [Jugador]
laSelesio = [pupiSalmeron, garrafaSanchez, satanasPaez, caniete, bigliaBurro]

-- Punto 1 --

-- a) Queremos saber los nombres de los jugadores de un equipo que jugaron todos los partidos al menos una cantidad de minutos, donde esa cantidad de minutos sea parametrizable. 
--Por ejemplo, si elegimos que la cantidad de minutos sea 45, puede haber jugado 45, 46 ó los 90, eso alcanza.

minutosJugados :: Number -> [Jugador] -> [String]
minutosJugados unMinuto = map nombre . filter (all (>= unMinuto) . map minutos . partidos) 

-- b) Queremos saber cuántos jugadores de un equipo marcaron goles en todos los partidos. --

metieronGoles :: [Jugador] -> Number
metieronGoles  = length . filter (all (> 0) . map goles . partidos)

-- c) Queremos saber si todos los jugadores de un equipo que tienen más de un mínimo de puntos de habilidad son volantes, donde ese mínimo es confi gurable. 
-- Ojo: si elijo que el mínimo sea 35, si hay 2 volantes que tienen 40 y 50 puntos de habilidad y un delantero con 50 puntos de habilidad, la condición no se cumple: todos los jugadores deben ser únicamente volantes.

habilidadMinima :: Number -> [Jugador] -> Bool
habilidadMinima puntosMinimos =  all ((==Volante) . puesto) . (filter ((> puntosMinimos) . habilidad))


-- Punto 2 --

type Tecnico = Jugador -> Jugador

bielsa :: Tecnico
bielsa jugador = jugador {velocidad = velocidad jugador * 1.5,habilidad = habilidad jugador - 10}

gago :: Tecnico
gago jugador 
    | puesto jugador == Volante = jugador {puesto = Defensor}
    | puesto jugador == Delantero = jugador {puesto = Volante}
    | otherwise = jugador

menotti :: Number -> Tecnico 
menotti puntos jugador = jugador {nombre = "Mr. " ++ nombre jugador, habilidad = habilidad jugador + puntos}

bertolotti :: Tecnico
bertolotti = menotti 10

vangaal :: Tecnico
vangaal jugador = jugador

-- (vangaal . menotti X . bielsa) jugador --
-- (vangaal . menotti 10 . bielsa) pupiSalmeron --


-- Punto 3.1 --

jugadorBueno :: Jugador -> Bool
jugadorBueno jugador = habilidad jugador > velocidad jugador || puesto jugador == Volante

cantidadBuenos :: [Jugador] -> Number
cantidadBuenos = length . filter jugadorBueno

mejoraTecnico :: Tecnico -> [Jugador] -> Bool
mejoraTecnico tecnico equipo = cantidadBuenos (map tecnico equipo) > cantidadBuenos equipo

-- Punto 3.2 --

jugadorBueno2 :: Jugador -> Bool
jugadorBueno2 jugador = habilidad jugador > velocidad jugador || puesto jugador == Volante

cantidadBuenos2 :: [Jugador] -> Number
cantidadBuenos2 = length . filter jugadorBueno2

buenaEnsenanza :: [Tecnico] -> Jugador -> Bool
buenaEnsenanza tecnicos jugador = jugadorBueno2 (foldl (flip ($)) jugador tecnicos)

-- Punto 4 --
esImparable :: [Partido] -> Bool
esImparable [] = True
esImparable [_] = True
esImparable (partido1:partido2:cola) = goles partido1 <= goles partido2 && esImparable (partido2:cola)


--Punto 5-- 

--Si, termina dando flase gracias a Lazy evaluation ya que se cortocircuita en el &&. Converge. 