import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsJavalette
import LexJavalette
import ParJavalette
import ErrM

import TypeChecker
import ReturnChecker

check :: String -> IO ()
check s = case pProgram (myLexer s) of
    Bad err  -> do
        putStr "SYNTAX ERROR: "
        putStrLn err
        exitFailure
    Ok  tree -> case typecheck tree of
        Bad err -> do
            putStr "TYPE ERROR: "
            putStrLn err
            exitFailure
        Ok tree -> case returncheck tree of
            Bad err -> do
                putStr "RETURN ERROR: "
                putStrLn err
                exitFailure
            Ok _ -> putStrLn "--------------------The program is well typed"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: main <SourceFile>"
      exitFailure
