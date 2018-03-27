module Scheme.Eval where

import           Control.Monad.Except (throwError)

import           Scheme.Data
import           Scheme.Eval.BinOp
import           Scheme.Eval.UnOp

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =  [("string?", isString),
               ("number?", isNumber),
               ("symbol?", isSymbol),
               ("symbol->string", symbolToString),
               ("string->symbol", stringToSymbol),
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
               ("string>=?", strBoolBinOp (>=)),
               ("car", car),
               ("cdr", cdr),
               ("cons", cons),
               ("eq?", eqv),
               ("eqv?", eqv),
               ("equal?", equal)
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
             Bool True  -> eval conseq
             Bool False -> eval alt
             notBool    -> throwError $ TypeMismatch "boolean" notBool
eval (List (Atom func : args)) = mapM eval args >>= apply func  -- implies strict evaluation strategy
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
