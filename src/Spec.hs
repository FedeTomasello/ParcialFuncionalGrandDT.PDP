module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Punto 1a - nombresQueJugaronAlMenos" $ do

  -- pupiSalmeron:  (90,3),(90,2),(45,1) → mínimo 45 → cumple
  -- garrafaSanchez: (90,0),(90,1),(90,1) → mínimo 90 → cumple
  -- satanasPaez:   (90,0),(90,0),(90,0) → mínimo 90 → cumple
  -- caniete:       (0,0),(0,0),(45,1)   → tiene 0 min → NO cumple
  -- bigliaBurro:   (0,0),(0,0),(0,0)    → tiene 0 min → NO cumple

    it "con mínimo 45, devuelve los jugadores que siempre jugaron al menos 45 min" $ do
      minutosJugados 45 laSelesio `shouldBe` ["Pupi Salmeron", "Garrafa Sanchez", "Satanas Paez"]

    it "con mínimo 90, devuelve solo los que jugaron los 90 en todos los partidos" $ do
      minutosJugados 90 laSelesio `shouldBe` ["Garrafa Sanchez", "Satanas Paez"]

    it "con mínimo 0, devuelve todos (incluso los del banco)" $ do
      minutosJugados 0 laSelesio `shouldBe` ["Pupi Salmeron", "Garrafa Sanchez", "Satanas Paez", "Adolfo Caniete", "Lucas Biglia"]

    it "con equipo vacío, devuelve lista vacía" $ do
      minutosJugados 45 [] `shouldBe` []
    
  describe "Punto 1b - cantidadQueMarcaronEnTodos" $ do

    -- pupiSalmeron:   (90,3),(90,2),(45,1) → goles: 3,2,1 → todos > 0 → cumple
    -- garrafaSanchez: (90,0),(90,1),(90,1) → goles: 0,1,1 → tiene 0 → NO cumple
    -- satanasPaez:    (90,0),(90,0),(90,0) → goles: 0,0,0 → todos 0  → NO cumple
    -- caniete:        (0,0),(0,0),(45,1)   → goles: 0,0,1 → tiene 0  → NO cumple
    -- bigliaBurro:    (0,0),(0,0),(0,0)    → goles: 0,0,0 → todos 0  → NO cumple

    it "devuelve cuántos jugadores metieron goles en todos sus partidos" $ do
      metieronGoles laSelesio `shouldBe` 1

    it "con equipo vacío devuelve 0" $ do
      metieronGoles [] `shouldBe` 0
  
  describe "Punto 1c - habilidadMinima" $ do
  -- con mínimo 35, jugadores con habilidad > 35:
  --   pupiSalmeron (78, Delantero) → NO es volante → False
  --   garrafaSanchez (99, Volante) → es volante
  --   caniete (78, Volante)        → es volante
  -- con mínimo 80, jugadores con habilidad > 80:
  --   garrafaSanchez (99, Volante) → es volante → True
  -- con mínimo 100, nadie supera → all sobre lista vacía → True
    it "devuelve False si algún jugador con habilidad suficiente no es volante" $ do
      habilidadMinima 35 laSelesio `shouldBe` False

  it "devuelve True si todos los jugadores con habilidad suficiente son volantes" $ do
    habilidadMinima 80 laSelesio `shouldBe` True

  it "devuelve True si ningún jugador supera el mínimo (all sobre lista vacía)" $ do
    habilidadMinima 100 laSelesio `shouldBe` True

  it "con equipo vacío devuelve True" $ do
    habilidadMinima 35 [] `shouldBe` True
  


  describe "bielsa" $ do
    it "aumenta 50% la velocidad" $ do
      velocidad (bielsa pupiSalmeron) `shouldBe` 49.5  -- 33 * 1.5

    it "baja 10 puntos de habilidad" $ do
      habilidad (bielsa pupiSalmeron) `shouldBe` 68  -- 78 - 10

  describe "gago" $ do
    it "convierte volante a defensor" $ do
      puesto (gago garrafaSanchez) `shouldBe` Defensor

    it "convierte delantero a volante" $ do
      puesto (gago pupiSalmeron) `shouldBe` Volante

    it "no cambia al arquero" $ do
      puesto (gago (pupiSalmeron { puesto = Arquero })) `shouldBe` Arquero

    it "no cambia al defensor" $ do
      puesto (gago satanasPaez) `shouldBe` Defensor

  describe "menotti" $ do
    it "agrega Mr. al nombre" $ do
      nombre (menotti 10 pupiSalmeron) `shouldBe` "Mr. Pupi Salmeron"

    it "aumenta la habilidad según el parámetro" $ do
      habilidad (menotti 10 pupiSalmeron) `shouldBe` 88  -- 78 + 10

    it "es parametrizable" $ do
      habilidad (menotti 20 pupiSalmeron) `shouldBe` 98  -- 78 + 20

  describe "bertolotti" $ do
    it "aumenta siempre 10 de habilidad" $ do
      habilidad (bertolotti pupiSalmeron) `shouldBe` 88

    it "agrega Mr. al nombre igual que menotti 10" $ do
      nombre (bertolotti pupiSalmeron) `shouldBe` "Mr. Pupi Salmeron"

  describe "vangaal" $ do
    it "no modifica al jugador" $ do
      vangaal pupiSalmeron `shouldBe` pupiSalmeron

  describe "composición de técnicos" $ do
    it "bielsa luego menotti luego vangaal modifica velocidad, habilidad y nombre" $ do
      nombre (vangaal . menotti 10 . bielsa $ pupiSalmeron) `shouldBe` "Mr. Pupi Salmeron"
      habilidad (vangaal . menotti 10 . bielsa $ pupiSalmeron) `shouldBe` 78  -- 78 - 10 + 10
      velocidad (vangaal . menotti 10 . bielsa $ pupiSalmeron) `shouldBe` 49.5

  describe "Punto 3.1 - mejoraTecnico" $ do

  -- laSelesio buenos SIN técnico:
  -- pupiSalmeron  (78 > 33 → True)  → bueno
  -- garrafaSanchez (99 > 88 → True) → bueno
  -- satanasPaez   (10 > 11 → False, no es Volante → False) → NO bueno
  -- caniete       (78 > 77 → True)  → bueno
  -- bigliaBurro   (1 > 10 → False, es Volante → True) → bueno
  -- total buenos antes: 4

    it "bielsa mejora el equipo si aumenta la cantidad de buenos" $ do
      mejoraTecnico bielsa laSelesio `shouldBe` False
    -- bielsa sube velocidad y baja habilidad, puede empeorar jugadores

    it "vangaal no mejora el equipo (no cambia nada)" $ do
      mejoraTecnico vangaal laSelesio `shouldBe` False

    it "menotti 20 mejora el equipo si sube la habilidad de los no buenos" $ do
      mejoraTecnico (menotti 20) laSelesio `shouldBe` True
    -- satanasPaez pasa de habilidad 10 a 30, sigue siendo 10 < 11... 
    -- probá con un número que realmente mejore a satanasPaez

  describe "Punto 3.2 - buenaEnsenanza" $ do

    it "con lista vacía de técnicos, chequea si el jugador ya es bueno" $ do
      buenaEnsenanza [] pupiSalmeron `shouldBe` True   -- 78 > 33
      buenaEnsenanza [] satanasPaez  `shouldBe` False  -- 10 < 11, no es volante

    it "un técnico que mejora la habilidad puede hacer bueno a un jugador malo" $ do
      buenaEnsenanza [menotti 10] satanasPaez `shouldBe` True  -- 20 > 11, sigue malo... 
      buenaEnsenanza [menotti 50] satanasPaez `shouldBe` True   -- 60 > 11, ahora es bueno

    it "serie de técnicos se aplican en orden" $ do
      buenaEnsenanza [bielsa, menotti 50] satanasPaez `shouldBe` True



  -- pupiSalmeron:   goles: 3,2,1 → 3>2 → NO es imparable
  -- garrafaSanchez: goles: 0,1,1 → 0<=1, 1<=1 → ES imparable
  -- satanasPaez:    goles: 0,0,0 → 0<=0, 0<=0 → ES imparable
  -- caniete:        goles: 0,0,1 → 0<=0, 0<=1 → ES imparable
  -- bigliaBurro:    goles: 0,0,0 → 0<=0, 0<=0 → ES imparable

  it "no es imparable si los goles decrecen" $ do
    esImparable (partidos pupiSalmeron) `shouldBe` False  -- 3,2,1

  it "es imparable si los goles son iguales" $ do
    esImparable (partidos satanasPaez) `shouldBe` True  -- 0,0,0

  it "es imparable si los goles crecen o se mantienen" $ do
    esImparable (partidos garrafaSanchez) `shouldBe` True  -- 0,1,1

  it "es imparable con lista vacía" $ do
    esImparable [] `shouldBe` True

  it "es imparable con un solo partido" $ do
    esImparable [(90, 5)] `shouldBe` True

  it "no es imparable si hay una caída en el medio" $ do
    esImparable [(90,0),(90,1),(90,0)] `shouldBe` False  -- 0,1,0