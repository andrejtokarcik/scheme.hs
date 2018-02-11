module Scheme.Parser where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.Error (throwError)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Scheme.Data
import Text.ParserCombinators.Parsec
import qualified Data.List.NonEmpty as NonEmpty

-----
-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
-----

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol <|> char '#')
              return . Atom $ first : rest
  where
      symbol :: Parser Char
      symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseBool :: Parser LispVal
parseBool = char '#' >>
            ((char 't' >> return (Bool True))
         <|> (char 'f' >> return (Bool False)))

parseNumber :: Parser LispVal
parseNumber = liftM Number $ (char '#' >> (bin <|> oct <|> hex <|> dec)) <|> noPrefix
  where
      bin = char 'b' >>
            fromJust . readBin <$> many1 (oneOf ['0', '1'])
      oct = char 'o' >>
            read . ("0o"++) <$> many1 (oneOf ['0'..'7'])
      hex = char 'x' >>
            read . ("0x"++) <$> many1 (digit <|> oneOf (['a'..'f'] ++ ['A'..'F']))
      dec = char 'd' >> noPrefix
      noPrefix = read <$> many1 digit

parseChar :: Parser LispVal
parseChar = string "#\\" >> Char <$> choice (zipWith replace codes replacements)
  where
      replace code replacement = string code >> return replacement
      codes        = [" ", "space", "newline", "(", ")"] ++ chunksOf 1 (['a'..'z'] ++ ['A'..'Z'])
      replacements = [' ', ' ', '\n', '(', ')'] ++ ['a'..'z'] ++ ['A'..'Z']

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many stringElem
                char '"'
                return $ String x
  where
      stringElem :: Parser Char
      stringElem =  (char '\\' >> choice (zipWith replace codes replacements))
                <|> noneOf ['"']
      -- http://codereview.stackexchange.com/questions/2406/parsing-strings-with-escaped-characters-using-parsec
      replace code replacement = char code >> return replacement
      codes        = ['n',  'r',  't',  '\\', '\"']
      replacements = ['\n', '\r', '\t', '\\', '\"']

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseList :: Parser LispVal
parseList = liftM List (parseExpr `sepBy` spaces)

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- parseExpr `endBy` spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList (NonEmpty.fromList head) tail

parseExpr :: Parser LispVal
parseExpr =  try parseAtom
         <|> try parseBool
         <|> try parseNumber
         <|> try parseChar
         <|> try parseString
         <|> try parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> ThrowsError LispVal
readExpr = either (throwError . Parser) return
         . parse parseExpr "lisp"