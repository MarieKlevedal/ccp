import System.Environment --(getArgs)
import System.Exit --(exitFailure)
import System.IO
import System.FilePath.Posix
import System.Process
import Control.Exception

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
        [file] -> check file
        _      -> putStrLn "Give Main a file!" >> exitFailure
      
-- check takes the string representation of the test program as input. It 
-- lexes and parses it into an AST and then typechecks and returnchecks it.
check :: FilePath -> IO ()
check file = do
    s <- readFile file
    case check' s of
        Bad err  -> hPutStr stderr ("ERROR\n" ++ err ++ "\n") >> exitFailure
        Ok  tree -> do
            --compileCode file tree
            hPutStr stderr ("OK\n" ++ printTree tree) >> exitSuccess

check' :: String -> Err Program
check' s = do
    pTree <- pProgram (myLexer s)
    tTree <- typecheck pTree
    returncheck tTree
    return $ alphaRen tTree

compileCode :: FilePath -> Program -> IO ()
compileCode file prog = do
    let code   = codeGen prog               -- get llvm code (string)
    let llFile = replaceExtension file "ll" -- creates ll file and path to it
    writeFile llFile code                   -- put compiled code in ll file
    
    let bcFile = (dropExtension file) ++ ".bc"
    
    res <- try $ (callCommand $ "llvm-as " ++ llFile) >> 
                 (callCommand $ "llvm-link " ++ bcFile ++ " lib/runtime.bc -o main.bc") 
    case (res :: Either IOError ())  of
        Right _ -> do              
            callCommand "llc -filetype=obj main.bc"
            callCommand "gcc main.o"
            return ()
        Left _ -> do
            hPutStr stderr ("ERROR\n" ++ "Couldn't compile llvm file\n") >> exitFailure
    
    

