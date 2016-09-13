import System.Environment (getArgs)

import Eval
import Parser

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
