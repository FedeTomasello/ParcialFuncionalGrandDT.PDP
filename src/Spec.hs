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