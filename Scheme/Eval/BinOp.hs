-- | Evaluation of binary operations.
module Scheme.Eval.BinOp
    ( numNumBinOp
    , numBoolBinOp
    , boolBoolBinOp
    , strBoolBinOp
    , cons
    , eqv
    , equal
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Error (catchError, throwError)
import Data.List.NonEmpty (NonEmpty (..), (<|), toList)

import Scheme.Data

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
          --unpackBool (List [b]) = unpackBool b   -- not supported by Scheme?
          unpackBool notBool    = throwError $ TypeMismatch "boolean" notBool

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = binOpOnAType unpackStr Bool
    where unpackStr :: LispVal -> ThrowsError String
          unpackStr (String s) = return s
          unpackStr (Number s) = return $ show s
          unpackStr (Bool s)   = return $ show s
          --unpackStr (List [s]) = unpackStr s     -- not supported by Scheme?
          unpackStr notStr     = throwError $ TypeMismatch "string" notStr

cons :: [LispVal] -> ThrowsError LispVal
cons = binOp $ (return.) . cons'
    where cons' x (List [])             = List [x]
          cons' x (List y)              = List (x : y)
          cons' x (DottedList xs xlast) = DottedList (x <| xs) xlast
          cons' x y                     = DottedList (x :| []) y

eqv :: [LispVal] -> ThrowsError LispVal
eqv = binOp $ \x y -> return . Bool $ eqv' x y
    where eqv' x y = show x == show y  -- HACK

equal :: [LispVal] -> ThrowsError LispVal
equal xs = binOp equal' xs <|> return (Bool False)
    where equal' :: LispVal -> LispVal -> ThrowsError LispVal  -- HACK
          equal' (List xs) (List ys) = if length xs /= length ys
                                        then return (Bool False)
                                        else collectBool <$> mapM (uncurry equal') (zip xs ys)
          equal' (DottedList xs x) (DottedList ys y) = equal' (List $ (toList xs) ++ [x])
                                                              (List $ (toList ys) ++ [y])
          equal' x y = let xs = [x,y] in strBoolBinOp (==) xs <|> eqv xs

          collectBool :: [LispVal] -> LispVal  -- partial
          collectBool = Bool . (all $ \(Bool b) -> b)

          a <|> e = a `catchError` const e  -- as though Alternative for Either?
