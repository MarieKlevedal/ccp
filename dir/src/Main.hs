import System.Environment --(getArgs)
import System.Exit --(exitFailure)
import System.IO

import AbsJavalette
import LexJavalette
import ParJavalette
import PrintJavalette
import ErrM

import TypeChecker
import ReturnChecker
import AlphaRen
import CodeGen

-- check takes the string representation of the test program as input. It 
-- lexes and parses it into an AST and then typechecks and returnchecks it.
-- If any of the phases report an error, an "ERROR" followed by a newline
-- and a error message is printed to standard error and then exits.
check :: String -> IO ()
check s = case compile of
    Bad err -> hPutStr stderr ("ERROR\n" ++ err) >> exitFailure
    Ok  str -> hPutStr stderr ("OK\n\n" ++ str)    >> exitSuccess
  where
    compile :: Err String
    compile = do
        pTree <- pProgram (myLexer s)
        tTree <- typecheck pTree
        returncheck tTree
        let renamedTree = alphaRen tTree
        let code = codeGen renamedTree
        return code

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
