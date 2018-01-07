module Eval.BinOp  -- TODO rename to BinaryOp?
    ( numNumBinOp
    , numBoolBinOp
    , boolBoolBinOp
    , strBoolBinOp
    ) where

import Control.Monad.Error (throwError)

import Internal

binOp :: (LispVal -> ThrowsError a) -> (b -> LispVal) -> (a -> a -> b) -> [LispVal] -> ThrowsError LispVal
binOp unpack pack op [x, y] = do x' <- unpack x
                                 y' <- unpack y
                                 return . pack $ x' `op` y'
binOp unpack pack _  params = throwError $ NumArgs 2 params

binOpOnNums :: (b -> LispVal) -> (Integer -> Integer -> b) -> [LispVal] -> ThrowsError LispVal
binOpOnNums = binOp unpackNum
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
boolBoolBinOp = binOp unpackBool Bool
    where unpackBool :: LispVal -> ThrowsError Bool
          unpackBool (Bool b)   = return b
          --unpackBool (List [b]) = unpackBool b
          unpackBool notBool    = throwError $ TypeMismatch "boolean" notBool

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = binOp unpackStr Bool
    where unpackStr :: LispVal -> ThrowsError String
          unpackStr (String s) = return s
          unpackStr (Number s) = return $ show s
          unpackStr (Bool s)   = return $ show s
          --unpackStr (List [s]) = unpackStr s
          unpackStr notStr     = throwError $ TypeMismatch "string" notStr
