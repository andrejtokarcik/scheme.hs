import Control.Monad (liftM)
import Control.Monad.Error (catchError)
import System.Environment (getArgs)

import Scheme.Eval
import Scheme.Internal
import Scheme.Parser

main :: IO ()
main = do getArgs >>= runWithArgs
  where runWithArgs [arg] = putStrLn . extractValue . trapError . evalInput $ arg
        runWithArgs _     = showHelp

evalInput :: String -> ThrowsError String
evalInput arg = liftM show $ readExpr arg >>= eval

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = error "Call just after trapError"

showHelp :: IO ()
showHelp = putStrLn "Run with a LISP expression as an argument"
