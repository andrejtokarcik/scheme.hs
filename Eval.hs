module Eval where

import Internal

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("remainder", numericBinOp rem)]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op params = Number $ foldl1 op $ map unpackNum params
  where
    unpackNum :: LispVal -> Integer
    unpackNum (Number n) = n
    {-
    unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                              if null parsed
                                  then error "meh unpackNum String"
                                  else fst $ parsed !! 0
    unpackNum (List [n]) = unpackNum n
    unpackNum _ = error "meh unpackNum _"
    -}

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (error "meh apply") ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool   _) = val
eval (List (Atom "quote" : [val])) = val
eval (List (Atom func    : args))  = apply func $ map eval args
