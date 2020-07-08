import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexGrammar
import ParGrammar
import SkelGrammar
import AbsGrammar
import TypeChecking
import Type
import TACGen

import ErrM
import Data.Map

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int


putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()


runFile :: Verbosity -> ParseFun (Program Posn) -> FilePath -> IO ()
runFile v p f = readFile f >>= run v p


run :: Verbosity -> ParseFun (Program Posn) -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse Failed!\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure

           Ok  tree -> do putStrLn ""
                          let typed = computeProgram tree initialEnv in
                
                            case typed of
                              Prog (Errs strs) _ -> sequence (Prelude.map putStrLn strs)
                              _ -> printTAC $ getTAC $ getTACFromProgram typed
                             
                          --putStrLn  (show(computeProgram tree initialEnv))
                          exitSuccess


-- Initial environment
initialEnv = fromList [ (compStrCall, [Fun (Pn 0 1 1) [TypeChecking.Param (SimpTyp T_String) (Pn 0 1 1) Modality1 "" dimPointer, TypeChecking.Param (SimpTyp T_String) (Pn 0 1 1) Modality1 "" dimPointer] (SimpTyp T_Int) dimInt]), ("writeInt", [Fun (Pn 0 1 1) [TypeChecking.Param (SimpTyp T_Int) (Pn 0 1 1) Modality1 "" dimInt, TypeChecking.Param (SimpTyp T_String) (Pn 0 1 1) Modality1 "" dimPointer] (SimpTyp T_Void) dimVoid]), ("writeChar", [Fun (Pn 0 1 1) [TypeChecking.Param (SimpTyp T_Char) (Pn 0 1 1) Modality1 "" dimChar, TypeChecking.Param (SimpTyp T_String) (Pn 0 1 1) Modality1 "" dimPointer] (SimpTyp T_Void) dimVoid]), ("writeFloat", [Fun (Pn 0 1 1) [TypeChecking.Param (SimpTyp T_Float64) (Pn 0 1 1) Modality1 "" dimFloat64, TypeChecking.Param (SimpTyp T_String) (Pn 0 1 1) Modality1 "" dimPointer] (SimpTyp T_Void) dimVoid]), ("writeString", [Fun (Pn 0 1 1) [TypeChecking.Param (SimpTyp T_String) (Pn 0 1 1) Modality1 "" dimPointer, TypeChecking.Param (SimpTyp T_String) (Pn 0 1 1) Modality1 "" dimPointer] (SimpTyp T_Void) dimVoid]), ("readInt", [Fun (Pn 0 1 1) [TypeChecking.Param (SimpTyp T_String) (Pn 0 1 1) Modality1 "" dimPointer] (SimpTyp T_Int) dimInt]), ("readFloat", [Fun (Pn 0 1 1) [TypeChecking.Param (SimpTyp T_String) (Pn 0 1 1) Modality1 "" dimPointer] (SimpTyp T_Float64) dimFloat64]), ("readChar", [Fun (Pn 0 1 1) [TypeChecking.Param (SimpTyp T_String) (Pn 0 1 1) Modality1 "" dimPointer] (SimpTyp T_Char) dimChar]), ("readString", [Fun (Pn 0 1 1) [TypeChecking.Param (SimpTyp T_String) (Pn 0 1 1) Modality1 "" dimPointer] (SimpTyp T_String) dimPointer])]


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= run 2 pProgram
    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs
