import System.Environment (getArgs)

import Parser

main :: IO ()
main = do
         (expr:_) <- getArgs
         putStrLn (readExpr expr)
