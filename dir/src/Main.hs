import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsJavalette
import LexJavalette
import ParJavalette
import ErrM

import TypeChecker

check :: String -> IO ()
check s = do
  case pProgram (myLexer s) of
    Bad err  -> do
      putStr "SYNTAX ERROR: "
      putStrLn err
      exitFailure
    Ok  tree -> do
      case typecheck tree of
        Bad err -> do
          putStr "TYPE ERROR: "
          putStrLn err
          exitFailure
        Ok _ -> putStrLn "------------------------The program is well typed"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: main <SourceFile>"
      exitFailure
