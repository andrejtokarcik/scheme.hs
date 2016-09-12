module Main where

import Control.Monad (liftM)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first : rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom
  where
      symbol :: Parser Char
      symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseNumber :: Parser LispVal
parseNumber = -- liftM (Number . read) $ many1 digit
              many1 digit >>= return . Number . read

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
         (expr:_) <- getArgs
         putStrLn (readExpr expr)
