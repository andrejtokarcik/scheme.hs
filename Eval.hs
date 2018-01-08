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
               ("cdr", cdr),
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
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", cond, conseq, alt]) =
     do result <- eval cond
        case result of
             Bool False -> eval alt
             _          -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func  -- implies strict evaluation strategy
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
