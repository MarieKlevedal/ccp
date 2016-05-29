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


-- main requires one single argument and passes it as argument to compileFile
main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> compileFile file
        _      -> putStrLn "Give Main a file!" >> exitFailure

        
-- compileFile requires a Javalette program file as argument, checks that it is
-- legal by calling check and then compiles it by calling compileFile. It gives 
-- an error message and exits with failure if anything goes wrong; otherwise, it
-- prints OK and exits with success.
compileFile :: FilePath -> IO ()
compileFile file = do
    s <- readFile file
    case check s of
        Bad err  -> hPutStr stderr ("ERROR\n" ++ err ++ "\n") >> exitFailure
        Ok  tree -> do
            compileCode file tree
            hPutStr stderr "OK\n" >> exitSuccess

            
-- check takes the string representation of the test program as input. It 
-- lexes and parses it into an AST and then type checks, return checks and alpha
-- renames it. If the program passes the tests, an alpha renamed, type annotated
-- AST is returned; otherwise, an error.
check :: String -> Err Program
check s = do
    pTree <- pProgram (myLexer s)
    tTree <- typecheck pTree
    returncheck tTree
    aTree <- alphaRen tTree
    return $ aTree

    
-- copileFile takes a JavaLette program file and an AST represeting the same 
-- program. It calls codeGen to create LLVM code which it translates to bitcode
-- format, links it with the runtime file and produces an output file. If 
-- anything goes wrong, it prints an error and exits with failure.
compileCode :: FilePath -> Program -> IO ()
compileCode file prog = do
    let code   = codeGen prog               -- get llvm code (string)
    let llFile = replaceExtension file "ll" -- create .ll file and path to it
    writeFile llFile code                   -- put compiled code in .ll file
    
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
    
    

