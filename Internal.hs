module Internal where

import Control.Monad.Error
import Data.Foldable (Foldable, toList)
import Data.List.NonEmpty (NonEmpty)
import Text.ParserCombinators.Parsec (ParseError)

data LispVal = Atom String
             | Bool Bool
             | Number Integer
             | Char Char
             | String String
             | List [LispVal]
             | DottedList (NonEmpty LispVal) LispVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Number contents) = show contents
showVal (Char char) = "#\\" ++ [char]
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (List []) = "NIL"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where
    show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

unwordsList :: (Foldable f) => f LispVal -> String
unwordsList = unwords . map showVal . toList

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values: " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default message)             = "(" ++ message ++ ")"

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg = Default "a LispError without message"
    strMsg = Default

-- TODO don't require precisely ThrowsError, generalise parameters to the MonadError class
-- + https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell
type ThrowsError = Either LispError
