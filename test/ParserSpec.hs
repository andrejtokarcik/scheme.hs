module ParserSpec (spec) where

import Test.Hspec

import Scheme.Data
import Scheme.Parser

spec ∷ Spec
spec =
  describe "readExpr" $ do
    let
      showAndReadExpr ∷ Show a ⇒ a → ThrowsError LispVal
      showAndReadExpr = readExpr . show

    it "parses string" $ do
      let string = "this is a string"
      showAndReadExpr string `shouldBe` Right (String string)

    it "parses number" $ do
      let number = 25
      showAndReadExpr number `shouldBe` Right (Number number)

    it "parses symbol" $ do
      let symbol = "symbol"
      readExpr symbol `shouldBe` Right (Atom symbol)
