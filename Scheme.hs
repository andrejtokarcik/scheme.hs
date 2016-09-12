module Main where
import System.Environment
import System.IO
 
main :: IO ()
main = do
    {-
    args <- getArgs
    -- putStrLn ("Hello, " ++ args !! 0 ++ " " ++ args !! 1)
    putStrLn $ args !! 0 ++ " + " ++ args !! 1 ++ " = " ++
      show ((read $ args !! 0) + (read $ args !! 1))
    -}
    putStr "Name: "
    hFlush stdout
    name <- getLine
    putStrLn $ "Hello, " ++ name
