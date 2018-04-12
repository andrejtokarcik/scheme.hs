module Scheme.Parser where

import           Control.Applicative           ((<$>))
import           Control.Monad.Except          (throwError)
import           Data.Char                     (digitToInt, toLower, toUpper)
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Maybe                    (fromJust)
import           Data.Maybe                    (listToMaybe)
import           Numeric                       (readInt)
import           Text.ParserCombinators.Parsec

import           Scheme.Data

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
-----

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol <|> oneOf "+-@.")
               return . Atom $ first : rest
  where
      symbol = oneOf "!$%&*/:<=>?^_~"

parseBool :: Parser LispVal
parseBool = char '#' >>
            ((char 't' >> return (Bool True))
         <|> (char 'f' >> return (Bool False)))

parseNumber :: Parser LispVal
parseNumber = fmap Number $ (char '#' >> (bin <|> oct <|> hex <|> dec)) <|> noPrefix
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
parseChar = string "#\\" >>
            Char <$> (choice (zipWith replace codes replacements) <|> anyChar)
  where
      replace code replacement = caseInsensitiveString code >> return replacement
      codes        = ["space", "newline"]
      replacements = [' ', '\n']
      -- https://stackoverflow.com/questions/12937325/whats-the-cleanest-way-to-do-case-insensitive-parsing-with-text-combinators-par
      caseInsensitiveChar c   = char (toLower c) <|> char (toUpper c)
      caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- many stringElem
                 _ <- char '"'
                 return $ String x
  where
      stringElem :: Parser Char
      stringElem = (char '\\' >> choice (zipWith replace codes replacements))
                <|> noneOf ['"']
      -- http://codereview.stackexchange.com/questions/2406/parsing-strings-with-escaped-characters-using-parsec
      replace code replacement = char code >> return replacement
      codes        = ['n',  'r',  't',  '\\', '\"']
      replacements = ['\n', '\r', '\t', '\\', '\"']

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseList :: Parser LispVal
parseList = fmap List $ parseExpr `sepBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    first <- parseExpr `endBy1` spaces
    rest  <- char '.' >> spaces >> parseExpr
    return $ DottedList (NonEmpty.fromList first) rest

parseExpr :: Parser LispVal
parseExpr =  try parseAtom
         <|> try parseBool
         <|> try parseNumber
         <|> try parseChar
         <|> try parseString
         <|> try parseQuoted
         <|> do _ <- char '('
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x

readExpr :: String -> ThrowsError LispVal
readExpr = either (throwError . Parser) return
         . parse parseExpr "lisp"
