module Internal (module Internal, module Control.Monad.Error) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

data LispVal = Atom String
             | Bool Bool
             | Number Integer
             | Char Char
             | String String
             | List [LispVal]
             | DottedList [LispVal] LispVal
    deriving Show

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args" --; found values: " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg = Default "(a LispError without message)"
    strMsg = Default

-- TODO don't require precisely ThrowsError, generalise parameters to the MonadError class
-- + https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell
type ThrowsError = Either LispError
