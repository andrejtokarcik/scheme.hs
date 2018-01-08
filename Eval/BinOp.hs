-- | Evaluation of binary operations.
module Eval.BinOp
    ( numNumBinOp
    , numBoolBinOp
    , boolBoolBinOp
    , strBoolBinOp
    , cons
    , eqv
    ) where


import Control.Monad.Error (throwError)
import Data.List.NonEmpty (NonEmpty (..), (<|))

import Internal

binOp :: (LispVal -> LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
binOp op [x, y] = x `op` y
binOp _  params = throwError $ NumArgs 2 params

binOpOnAType :: (LispVal -> ThrowsError a) -> (b -> LispVal) -> (a -> a -> b) -> [LispVal] -> ThrowsError LispVal
binOpOnAType unpack pack op = binOp op'
    where op' x y = do x' <- unpack x
                       y' <- unpack y
                       return . pack $ x' `op` y'

binOpOnNums :: (b -> LispVal) -> (Integer -> Integer -> b) -> [LispVal] -> ThrowsError LispVal
binOpOnNums = binOpOnAType unpackNum
    where unpackNum :: LispVal -> ThrowsError Integer
          unpackNum (Number n)   = return n
          unpackNum s@(String n) = case reads n of [(n', _)] -> return n'
                                                   _         -> throwError $ TypeMismatch "number" s
          unpackNum (List [n])   = unpackNum n
          unpackNum notNum       = throwError $ TypeMismatch "number" notNum

numNumBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numNumBinOp = binOpOnNums Number

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinOp = binOpOnNums Bool

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = binOpOnAType unpackBool Bool
    where unpackBool :: LispVal -> ThrowsError Bool
          unpackBool (Bool b)   = return b
          --unpackBool (List [b]) = unpackBool b
          unpackBool notBool    = throwError $ TypeMismatch "boolean" notBool

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = binOpOnAType unpackStr Bool
    where unpackStr :: LispVal -> ThrowsError String
          unpackStr (String s) = return s
          unpackStr (Number s) = return $ show s
          unpackStr (Bool s)   = return $ show s
          --unpackStr (List [s]) = unpackStr s
          unpackStr notStr     = throwError $ TypeMismatch "string" notStr

cons :: [LispVal] -> ThrowsError LispVal
cons = binOp $ (return.) . cons'
    where cons' x (List [])             = List [x]
          cons' x (List y)              = List (x : y)
          cons' x (DottedList xs xlast) = DottedList (x <| xs) xlast
          cons' x y                     = DottedList (x :| []) y

eqv :: [LispVal] -> ThrowsError LispVal
eqv = binOp $ \x y -> return . Bool $ x == y
