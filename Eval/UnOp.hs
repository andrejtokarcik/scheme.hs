module Eval.UnOp  -- TODO rename to UnaryOp?
    ( isString
    , isNumber
    , isSymbol
    , symbolToString
    , stringToSymbol
    , car
    ) where

import Control.Monad.Error (throwError)

import Internal

unOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unOp op [param] = op param
unOp op params  = throwError $ NumArgs 1 params

totalUnOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
totalUnOp op = unOp (return . op)

isString = totalUnOp isString'
    where isString' :: LispVal -> LispVal
          isString' (String _) = Bool True
          isString' _          = Bool False

isNumber = totalUnOp isNumber'
    where isNumber' :: LispVal -> LispVal
          isNumber' (Number _) = Bool True
          isNumber' _          = Bool False

isSymbol = totalUnOp isSymbol'
    where isSymbol' :: LispVal -> LispVal
          isSymbol' (Atom _) = Bool True
          isSymbol' _        = Bool False

symbolToString = unOp symbolToString'
    where symbolToString' :: LispVal -> ThrowsError LispVal
          symbolToString' (Atom a) = return $ String a
          symbolToString' notAtom  = throwError $ TypeMismatch "symbol" notAtom

stringToSymbol = unOp stringToSymbol'
    where stringToSymbol' :: LispVal -> ThrowsError LispVal
          stringToSymbol' (String s) = return $ Atom s
          stringToSymbol' notString  = throwError $ TypeMismatch "string" notString

car = unOp car'
    where car' :: LispVal -> ThrowsError LispVal
          car' (List (x:xs))         = return x
          car' (DottedList (x:xs) _) = return x
          car' badArg                = throwError $ TypeMismatch "pair" badArg
