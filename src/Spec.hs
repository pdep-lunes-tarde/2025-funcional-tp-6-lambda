module Spec where

import Control.Exception (evaluate)
import Library
import PdePreludat
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  let hamburguesaPollo = Hamburguesa {precioBase = 15, ingredientes = [Pan, Pollo, QuesoDeAlmendras, Pan]}
  let hamburguesaPolloCarne = Hamburguesa {precioBase = 16, ingredientes = [Pan, Pollo, Carne, QuesoDeAlmendras, Pan]}

  describe "agrandar" $ do
    it "Le damos una hamburguesa de carne y le agrega una feta de carne" $ do
      agrandar cuartoDeLibra `shouldBe` Hamburguesa {precioBase = 20, ingredientes = [Carne, Pan, Carne, Cheddar, Pan]}
    it "Le damos una hamburguesa de pollo y le agrega una feta de pollo" $ do
      agrandar hamburguesaPollo `shouldBe` Hamburguesa {precioBase = 15, ingredientes = [Pollo, Pan, Pollo, QuesoDeAlmendras, Pan]}
    it "Le damos una hamburguesa con pollo y carne y le agrega carne pues es el primero en ser evaluado" $ do
      agrandar hamburguesaPolloCarne `shouldBe` Hamburguesa {precioBase = 36, ingredientes = [Carne, Pan, Pollo, Carne, QuesoDeAlmendras, Pan]}

  describe "agregarIngrediente" $ do
    it "Le damos una hamburguesa, un ingrediente y le agrega el ingrediente a la hamburguesa" $ do
      agregarIngrediente Curry hamburguesaPollo `shouldBe` Hamburguesa {precioBase = 20, ingredientes = [Curry, Pan, Pollo, QuesoDeAlmendras, Pan]}

  describe "descuento" $ do
    it "Le damos una hamburguesa, un porcentaje de descuento y devolvemos la hamburguesa con descuento" $ do
      descuento 20 cuartoDeLibra `shouldBe` Hamburguesa {precioBase = 16, ingredientes = [Pan, Carne, Cheddar, Pan]}

  describe "pdepBurger" $ do
    it "Retorna un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento" $ do
      pdepBurger `shouldBe` 110

  describe "dobleCuarto" $ do
    it "Retorna un cuarto de libra con carne y cheddar" $ do
      dobleCuarto `shouldBe` Hamburguesa {precioBase = 84, ingredientes = Carne : Cheddar : ingredientes cuartoDeLibra}
  describe "bigPdep" $ do
    it "Retorna un doble cuarto con curry" $ do
      bigPdep `shouldBe` Hamburguesa {precioBase = 89, ingredientes = Curry : Carne : Cheddar : ingredientes cuartoDeLibra}