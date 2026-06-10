module Library where
import PdePreludat

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