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
-- If any of the phases report an error, an "ERROR" followed by a newline
-- and a error message is printed to standard error and then exits.
check :: String -> IO ()
check s = case pProgram (myLexer s) of
    Bad err  -> do
        hPutStr stderr $ "ERROR\nSyntax error: " ++ err ++ "\n"
        exitFailure
    Ok  tree -> case typecheck tree of
        Bad err -> do
            hPutStr stderr $ "ERROR\nType error: " ++ err ++ "\n"
            exitFailure
        Ok typeAnnoTree -> case returncheck typeAnnoTree of
            Bad err -> do
                hPutStr stderr $ "ERROR\nReturn error: " ++ err ++ "\n"
                exitFailure
            Ok _ -> case codeGen typeAnnoTree of
                Bad err -> do
                    hPutStr stderr $ "ERROR\nReturn error: " ++ err ++ "\n"
                    exitFailure
                Ok str  -> do
                    hPutStr stderr "OK\n"
                    putStrLn str
                    exitSuccess

-- main reads a file and returns its contents as a string input for the
-- check function.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: main <SourceFile>"
      exitFailure
