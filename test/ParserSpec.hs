module ParserSpec (spec) where

import           Data.Char
import           Test.Hspec
import           Test.QuickCheck

import           Scheme.Data
import           Scheme.Parser

spec ∷ Spec
spec =
  describe "readExpr" $ do
    it "parses any positive decimal number" $ property $
      -- Numbers prepended with explicit -/+ sign are currently
      -- interpreted as Atoms, not Numbers.  FIXME?
      \ (NonNegative i) → (readExpr . show) i === Right (Number i)

    it "parses any string without escape sequences" $ property $
      forAll stringWithoutEscapes $ \ s →
        readExpr ("\"" ++ s ++ "\"") === Right (String s)

    it "parses string with all escape sequences" $ do
      let stringWithEscapes = "\n\r\t\\\"" in
        readExpr (show stringWithEscapes) `shouldBe` Right (String stringWithEscapes)

    it "parses any atom identifier" $ property $
      forAll atomId $ \ s →
        readExpr s === Right (Atom s)

stringWithoutEscapes ∷ Gen String
stringWithoutEscapes = arbitrary `suchThat` (all (`notElem` ['\\', '"']))

atomId ∷ Gen String
atomId = do c  ← oneof [letter, special]
            cs ← listOf $ oneof [letter, digit, special]
            return (c:cs)
    where letter = arbitrary `suchThat` isLetter
          digit = arbitrary `suchThat` isDigit
          -- cf. http://www.schemers.org/Documents/Standards/R5RS/HTML/
          -- nonterminals ⟨special initial⟩ and ⟨special subsequent⟩
          special = elements "!$%&|*+-/:<=>?@^_~"
