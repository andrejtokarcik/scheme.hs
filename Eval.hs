module Eval where

import Internal

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =  [("+", numericBinOp (+)),
               ("-", numericBinOp (-)),
               ("*", numericBinOp (*)),
               ("/", numericBinOp div),
               ("mod", numericBinOp mod),
               ("quotient", numericBinOp quot),
               ("remainder", numericBinOp rem),
               ("string?", totalUnOp isString),
               ("number?", totalUnOp isNumber),
               ("symbol?", totalUnOp isSymbol),
               ("symbol->string", unOp symbolToString),
               ("string->symbol", unOp stringToSymbol)]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op params@[_, _] = mapM unpackNum params >>= return . Number . foldl1 op
    where unpackNum :: LispVal -> ThrowsError Integer
          unpackNum   (Number n)  = return n
          unpackNum s@(String s') = unpackParsedNum $ reads s'
                 where unpackParsedNum [(n, _)] = return n
                       unpackParsedNum _        = throwError $ TypeMismatch "number" s
          unpackNum (List [n])    = unpackNum n -- maybe don't require singletons?
          unpackNum notNum        = throwError $ TypeMismatch "number" notNum
numericBinOp _  params        = throwError $ NumArgs 2 params

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
symbolToString notAtom  = throwError $ TypeMismatch "atom" notAtom

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String s) = return $ Atom s
stringToSymbol notString  = throwError $ TypeMismatch "string" notString

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                        ($ args)
                        (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool   _) = return val
eval (List (Atom "quote" : [val])) = return val
eval (List (Atom func    : args))  = mapM eval args >>= apply func
