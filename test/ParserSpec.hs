module ParserSpec (spec) where

import           Data.Char
import           Data.Foldable   (sequenceA_)
import           Data.Maybe
import           Numeric         (readHex, readOct)
import           Test.Hspec
import           Test.QuickCheck

import           Scheme.Data
import           Scheme.Parser

spec ∷ Spec
spec =
  describe "readExpr" $ do
    it "parses any decimal number (without prefix)" $ property $
      -- Numbers prepended with explicit -/+ sign are currently
      -- interpreted as Atoms, not Numbers.  FIXME?
      forAll decimal $ \ s → readExpr s === Right (Number $ read s)

    it "parses any decimal number (prefixed with #d)" $ property $
      forAll decimal $ \ s → readExpr ("#d" ++ s) === Right (Number $ read s)

    it "parses any binary number (prefixed with #b)" $ property $
      forAll binary $ \ s →
        readExpr ("#b" ++ s) === Right (Number . fromJust $ readBin s)

    it "parses any octal number (prefixed with #o)" $ property $
      forAll octal $ \ s →
        readExpr ("#o" ++ s) === Right (Number . (\ [(x,"")] → x) $ readOct s)

    it "parses any hexadecimal number (prefixed with #x)" $ property $
      forAll hexadecimal $ \ s →
        readExpr ("#x" ++ s) === Right (Number . (\ [(x,"")] → x) $ readHex s)

    it "parses any string without escape sequences" $ property $
      forAll stringWithoutEscapes $ \ s →
        readExpr ("\"" ++ s ++ "\"") === Right (String s)

    it "parses string with all escape sequences" $ do
      let stringWithEscapes = "\n\r\t\\\""
      readExpr (show stringWithEscapes) `shouldBe` Right (String stringWithEscapes)

    it "parses any atom identifier" $ property $
      forAll atomId $ \ s → readExpr s === Right (Atom s)

    it "parses any character literal" $ property $
      forAll arbitraryPrintableChar $ \ c → readExpr ("#\\" ++ [c]) === Right (Char c)

    it "parses character names" $ do
      let checkParse (name, char) = readExpr ("#\\" ++ name) `shouldBe` Right (Char char)
      sequenceA_ $ map checkParse
        [("space", ' '), ("SPACE", ' '), ("newline", '\n'), ("NEWLINE", '\n')]

    it "parses any parenthesized list of atoms" $ do
      forAll parenthesizedList $ \ s → reportList s $
        case readExpr s of
          Right (List _) → True
          _              → False

    it "parses any dotted list of atoms" $ do
      forAll dottedList $ \ s → reportDotted s $
        case readExpr s of
          Right (DottedList _ _) → True
          _                      → False

    it "parses single-quote syntactic sugar" $ do
      forAll quoted $ \ s → reportQuoted s $
        case readExpr s of
          Right (List [Atom "quote", _]) → True
          _                              → False

    where
      counterexample' e s = counterexample ("Got " ++ show s ++ " instead of " ++ e)
      reportList   = counterexample' "parenthesized list"
      reportDotted = counterexample' "dotted list"
      reportQuoted = counterexample' "single-quoted datum"

stringWithoutEscapes ∷ Gen String
stringWithoutEscapes = arbitrary `suchThat` (all (`notElem` ['\\', '"']))

atomId ∷ Gen String
atomId = do c  ← oneof [letter, specialInit]
            cs ← listOf $ oneof [letter, digit, specialInit, specialSubs]
            return (c:cs)
    where letter = arbitrary `suchThat` isLetter
          digit = arbitrary `suchThat` isDigit
          -- cf. http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-10.html#%_sec_7.1.1
          -- nonterminals ⟨special initial⟩ and ⟨special subsequent⟩
          specialInit = elements "!$%&*/:<=>?^_~"
          specialSubs = elements "+-@."

decimalDigit ∷ Gen Char
decimalDigit = elements ['0'..'9']

decimal ∷ Gen String
decimal = listOf1 decimalDigit

binary ∷ Gen String
binary = listOf1 $ elements ['0', '1']

octal ∷ Gen String
octal = listOf1 $ elements ['0'..'7']

hexadecimal ∷ Gen String
hexadecimal = listOf1 $ oneof [decimalDigit, hexChar, toUpper <$> hexChar]
    where hexChar = elements ['a'..'f']

parenthesizedList ∷ Gen String
parenthesizedList = do atoms ← listOf1 atomId
                       return $ "(" ++ unwords atoms ++ ")"

dottedList ∷ Gen String
dottedList = do initAtoms ← listOf1 atomId
                lastAtom  ← atomId
                return $ "(" ++ unwords initAtoms ++ " . " ++ lastAtom ++ ")"

quoted ∷ Gen String
quoted = datum >>= \ x → return $ "'" ++ x
    where datum = oneof [atomId, decimal, parenthesizedList, dottedList]
