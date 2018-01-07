module Eval where

import Internal

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =  [("string?", totalUnOp isString),
               ("number?", totalUnOp isNumber),
               ("symbol?", totalUnOp isSymbol),
               ("symbol->string", unOp symbolToString),
               ("string->symbol", unOp stringToSymbol),
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

unOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unOp op [param] = op param
unOp op params  = throwError $ NumArgs 1 params

totalUnOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
totalUnOp op = unOp (return . op)

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _          = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _          = Bool False

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _        = Bool False

symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom a) = return $ String a
symbolToString notAtom  = throwError $ TypeMismatch "symbol" notAtom

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String s) = return $ Atom s
stringToSymbol notString  = throwError $ TypeMismatch "string" notString

binOp :: (LispVal -> ThrowsError a) -> (b -> LispVal) -> (a -> a -> b) -> [LispVal] -> ThrowsError LispVal
binOp unpack pack op [x, y] = do x' <- unpack x
                                 y' <- unpack y
                                 return . pack $ x' `op` y'
binOp unpack pack _  params = throwError $ NumArgs 2 params

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

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                        ($ args)
                        (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool   _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
