import System.Environment --(getArgs)
import System.Exit --(exitFailure)
import System.IO

import AbsJavalette
import LexJavalette
import ParJavalette
import ErrM

import TypeChecker
import ReturnChecker

-- check takes the string representation of the test program as input. It 
-- lexes and parses it into an AST and then typechecks and returnchecks it.
-- If any of the phases report an error, this is printed and TODO continue
check :: String -> IO ()
check s = case pProgram (myLexer s) of
    Bad err  -> do
        stderr "ERROR"
        stderr $ "Syntax error: " ++ err
        exitWith (ExitFailure 1)
    Ok  tree -> case typecheck tree of
        Bad err -> do
            stderr "ERROR"
            stderr $ "Type error: " ++ err
            exitWith (ExitFailure 1)
        Ok tree -> case returncheck tree of
            Bad err -> do
                stderr "ERROR"
                stderr $ "Return error: " ++ err
                exitWith (ExitFailure 1)
            Ok _ -> do
                stderr "OK"
                exitWith (ExitFailure 0)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: main <SourceFile>"
      exitFailure
