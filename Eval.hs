module Eval where

import Control.Monad.Error (throwError)

import Eval.BinOp
import Eval.UnOp
import Internal

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =  [("string?", isString),
               ("number?", isNumber),
               ("symbol?", isSymbol),
               ("symbol->string", symbolToString),
               ("string->symbol", stringToSymbol),
               ("car", car),
               ("+", numNumBinOp (+)),
               ("-", numNumBinOp (-)),
               ("*", numNumBinOp (*)),
               ("/", numNumBinOp div),
               ("mod", numNumBinOp mod),
               ("quotient", numNumBinOp quot),
               ("remainder", numNumBinOp rem),
               ("=", numBoolBinOp (==)),
               ("<", numBoolBinOp (<)),
               (">", numBoolBinOp (>)),
               ("/=", numBoolBinOp (/=)),
               (">=", numBoolBinOp (>=)),
               ("<=", numBoolBinOp (<=)),
               ("&&", boolBoolBinOp (&&)),
               ("||", boolBoolBinOp (||)),
               ("string=?", strBoolBinOp (==)),
               ("string<?", strBoolBinOp (<)),
               ("string>?", strBoolBinOp (>)),
               ("string<=?", strBoolBinOp (<=)),
               ("string>=?", strBoolBinOp (>=))
               ]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                        ($ args)
                        (lookup func primitives)

-- TODO partial evaluation (+ benchmark)
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool   _) = return val
eval (List [Atom "quote", val]) = return val   -- TODO as common function?
eval (List [Atom "if", pred, conseq, alt]) =   -- TODO as common function?
     do result <- eval pred
        case result of
             Bool False -> eval alt
             _          -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
