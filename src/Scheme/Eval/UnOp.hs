-- | Evaluation of unary operations.
module Scheme.Eval.UnOp
    ( isString
    , isNumber
    , isSymbol
    , symbolToString
    , stringToSymbol
    , car
    , cdr
    ) where

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Control.Monad.Except (throwError)

import Scheme.Data

unOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unOp op [param] = op param
unOp _  params  = throwError $ NumArgs 1 params

totalUnOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
totalUnOp op = unOp (return . op)

isString :: [LispVal] -> ThrowsError LispVal
isString = totalUnOp isString'
    where isString' (String _) = Bool True
          isString' _          = Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber = totalUnOp isNumber'
    where isNumber' (Number _) = Bool True
          isNumber' _          = Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol = totalUnOp isSymbol'
    where isSymbol' (Atom _) = Bool True
          isSymbol' _        = Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString = unOp symbolToString'
    where symbolToString' (Atom a) = return $ String a
          symbolToString' notAtom  = throwError $ TypeMismatch "symbol" notAtom

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol = unOp stringToSymbol'
    where stringToSymbol' (String s) = return $ Atom s
          stringToSymbol' notString  = throwError $ TypeMismatch "string" notString

car :: [LispVal] -> ThrowsError LispVal
car = unOp car'
    where car' (List (x : _))          = return x
          car' (DottedList (x :| _) _) = return x
          car' badArg                  = throwError $ TypeMismatch "pair" badArg

cdr :: [LispVal] -> ThrowsError LispVal
cdr = unOp cdr'
    where cdr' (List (_ : xs))          = return $ List xs
          cdr' (DottedList (_ :| xs) x) = return $ maybe x (flip DottedList x) (nonEmpty xs)
          cdr' badArg                   = throwError $ TypeMismatch "pair" badArg
