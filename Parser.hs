module Parser where

import Control.Monad (liftM)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec


-----
-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
-----

data LispVal = Atom String
             | Bool Bool
             | Number Integer
             | Char Char
             | String String
             | List [LispVal]
             | DottedList [LispVal] LispVal
  deriving Show

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter
              rest <- many (letter <|> digit <|> symbol)
              return . Atom $ first : rest
  where
      symbol :: Parser Char
      symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseBool :: Parser LispVal
parseBool =  (string "#t" >> return (Bool True))
         <|> (string "#f" >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = liftM Number $ (char '#' >> (bin <|> oct <|> hex <|> dec)) <|> noPrefix
  where
      bin = char 'b' >>
            many1 (oneOf ['0', '1']) >>= return . fromJust . readBin
      oct = char 'o' >>
            many1 (oneOf ['0'..'7']) >>= return . read . ("0o"++)
      hex = char 'x' >>
            many1 (digit <|> oneOf (['a'..'f'] ++ ['A'..'F'])) >>= return . read . ("0x"++)
      dec = char 'd' >> noPrefix
      noPrefix = many1 digit >>= return . read

parseChar :: Parser LispVal
parseChar = string "#\\" >> choice (zipWith replace codes replacements) >>= return . Char
  where
      replace code replacement = string code >> return replacement
      codes        = [" ", "space", "newline", "(", ")"] ++ chunksOf 1 (['a'..'z'] ++ ['A'..'Z'])
      replacements = [' ', ' ', '\n', '(', ')'] ++ ['a'..'z'] ++ ['A'..'Z']

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ stringElem
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

parseList :: Parser LispVal
parseList = liftM List (parseExpr `sepBy1` spaces)

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- parseExpr `endBy` spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

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

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val