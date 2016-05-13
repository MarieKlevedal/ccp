import System.Environment --(getArgs)
import System.Exit --(exitFailure)
import System.IO
import System.FilePath.Posix
import System.Process

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
--check :: String -> Err Program
check :: FilePath -> IO ()
check file = do
    s <- readFile file
    case check' s of
        Bad err  -> hPutStr stderr ("ERROR\n" ++ err) >> exitFailure
        Ok  tree -> do
            --putStrLn $ (printTree tree) ++ "\n\n"
            compileCode file tree
            hPutStr stderr "OK\n" >> exitSuccess

check' :: String -> Err Program
check' s = do
    pTree <- pProgram (myLexer s)
    tTree <- typecheck pTree
    returncheck tTree
    return $ alphaRen tTree

compileCode :: FilePath -> Program -> IO ()
compileCode file prog = do
    let name   = takeBaseName file          -- extract file name (string)
    let code   = codeGen prog               -- get llvm code (string)
    let dir    = takeDirectory file         -- get path (string)
    let llFile = replaceExtension file "ll" -- creates ll file and path to it
    writeFile llFile code                   -- put compiled code in ll file
    
    runCommand $ "llvm-as " ++ llFile 
    --let bcFile = (dropExtension file) ++ ".bc"
    --runCommand $ "llvm-link " ++ bcFile ++ " lib/runtime.bc -o main.bc" 
    --runCommand "llc -filetype=obj main.bc"
    --runCommand "gcc main.o"
    
    return ()
    

