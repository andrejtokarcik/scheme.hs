import           Control.Monad.Except (catchError)
import           System.Environment   (getArgs)

import           Scheme

main :: IO ()
main = getArgs >>= runWithArgs
  where runWithArgs [arg] = putStrLn . extractValue . trapError . evalInput $ arg
        runWithArgs _     = showHelp

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _)    = error "Call just after trapError"

showHelp :: IO ()
showHelp = putStrLn "Run with a LISP expression as an argument"
