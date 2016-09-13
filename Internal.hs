module Internal where

data LispVal = Atom String
             | Bool Bool
             | Number Integer
             | Char Char
             | String String
             | List [LispVal]
             | DottedList [LispVal] LispVal
  deriving Show
