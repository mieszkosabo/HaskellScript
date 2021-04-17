module Main where
import System.IO ( stdin, hGetContents, hPutStrLn, stderr, getContents, hPutStr )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Interpreter
import Types
import AbsHaskellScript
import ErrM
import ParHaskellScript ( pProgram, myLexer )


parse :: String -> IO ()
parse input =
  case pProgram (myLexer input) of
    (Ok parsedProg) -> do
      let Program stmts = parsedProg
      -- todo typechecking
      runtimeRes <- runHSI stmts
      case runtimeRes of
        Left _ -> do hPutStrLn stderr "Runtime Error"; exitFailure
        Right s -> do
          -- print s
          return ()
    (Bad _) -> hPutStrLn stderr "Error while parsing" >> exitFailure

parseFile :: String -> IO ()
parseFile filename = readFile filename >>= parse


main :: IO ()
main = do
  files <- getArgs
  mapM_ parseFile files
