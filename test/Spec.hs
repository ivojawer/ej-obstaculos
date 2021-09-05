module Main where

import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Tests de alto nivel" $ do
    it "puede llegar si da la suma" $
      puedeLlegar (0,0) (3,4) [] `shouldBe` True -- (0,1) -> (0,2) -> (0,4) -> (3,4)
    it "no puede llegar si no da la suma" $
      puedeLlegar (0,0) (0,3) [] `shouldBe` False
    it "si hay un obstaculo lo esquiva" $
      puedeLlegar (0,0) (3,4) [(0,1)] `shouldBe` True -- (1,0) -> (1,1) -> (3,1) -> (3,4)
    it "no puede llegar si da la suma pero hay un obstaculo" $
      puedeLlegar (0,0) (0,2) [(0,1)] `shouldBe` False
    it "si esta en el target entonces ya llego" $
      puedeLlegar (5,5) (5,5) [] `shouldBe` True
    it "si ya se paso entonces nunca va a llegar" $
      puedeLlegar (5,5) (2,2) [] `shouldBe` False
    it "traduce bien de enunciado a dominio" $
      entryPoint 0 0 3 4 [0,2] [1,2] `shouldBe` True