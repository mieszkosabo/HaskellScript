module Main where
import System.IO ( stdin, hGetContents, hPutStrLn, stderr, getContents, hPutStr )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Interpreter
import Types
import AbsHaskellScript
import ErrM
import ParHaskellScript ( pProgram, myLexer )


loadOverture :: IO ([Stmt] -> IO (Either RunTimeErrors ((Env, ReturnedValue), Store)))
loadOverture = do
  input <- readFile "lib/Overture.hss"
  let (Ok (Program stmts)) = pProgram (myLexer input)
  (Right ((env, _), store)) <- runHSI stmts
  return (runPreloadedHSI env store)


parse :: String -> IO ()
parse input =
  case pProgram (myLexer input) of
    (Ok parsedProg) -> do
      let Program stmts = parsedProg
      -- todo typechecking
      
      preloadedHSI <- loadOverture
      runtimeRes <- preloadedHSI stmts
      --runtimeRes <- runHSI stmts
      case runtimeRes of
        Left err -> do hPutStrLn stderr ("Runtime Error: " ++ show err); exitFailure
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
