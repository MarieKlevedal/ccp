import System.Environment --(getArgs)
import System.Exit --(exitFailure)
import System.IO
import System.FilePath.Posix

import AbsJavalette
import LexJavalette
import ParJavalette
import PrintJavalette
import ErrM

import TypeChecker
import ReturnChecker
import AlphaRen
import CodeGen

-- main reads a file and returns its contents as a string input for the
-- check function.
-- If any of the phases report an error, an "ERROR" followed by a newline
-- and a error message is printed to standard error and then exits.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            s <- readFile file
            case check s of
                Bad err  -> hPutStr stderr ("ERROR\n" ++ err) >> exitFailure
                Ok  tree -> do
                    compileCode file tree
                    hPutStr stderr "OK\n"  >> exitSuccess
                    --check file --readFile file >>= check
        _      -> do
            putStrLn "Give Main a file!"
            exitFailure
      
-- check takes the string representation of the test program as input. It 
-- lexes and parses it into an AST and then typechecks and returnchecks it.
check :: String -> Err Program
check s = do
    pTree <- pProgram (myLexer s)
    tTree <- typecheck pTree
    returncheck tTree
    let renamedTree = alphaRen tTree
    return renamedTree

compileCode :: FilePath -> Program -> IO ()
compileCode file prog = do
    let name   = takeBaseName file
    let code   = codeGen prog
    let dir    = takeDirectory file
    let lFile  = replaceExtension file "ll"
    writeFile lFile code
