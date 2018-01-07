import System.Environment (getArgs)

import Eval
import Internal
import Parser

main :: IO ()
main = getArgs >>= print . extractValue . trapError . evalInput . head

evalInput :: String -> ThrowsError String
evalInput arg = liftM show $ readExpr arg >>= eval

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
