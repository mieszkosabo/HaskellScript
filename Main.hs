module Main where
import System.IO ( stdin, hGetContents, hPutStrLn, stderr, getContents, hPutStr )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Interpreter
import TypeCheck
import Types
import AbsHaskellScript
import ErrM
import ParHaskellScript ( pProgram, myLexer )


loadOverture :: IO ([Stmt] -> IO (Either RunTimeErrors ((Env, ReturnedValue), Store)))
loadOverture = do
  input <- readFile "lib/Overture.hss"
  let (Ok (Program _ stmts)) = pProgram (myLexer input)
  (Right ((env, _), store)) <- runHSI stmts
  return (runPreloadedHSI env store)


loadOvertureTypes = do
  input <- readFile "lib/Overture.hss"
  let (Ok (Program _ stmts)) = pProgram (myLexer input)
  (Right (env, _)) <- runTypeCheck stmts
  return (runPreloadedTypeCheck env)


parse :: String -> IO ()
parse input =
  case pProgram (myLexer input) of
    (Ok parsedProg) -> do
      let Program _ stmts = parsedProg
      preloadedTypeCheck <- loadOvertureTypes
      typeCheckRes <- preloadedTypeCheck stmts
      case typeCheckRes of
        Left err -> hPutStrLn stderr $ "Type Error! " ++ show err
        Right _ -> do
          preloadedHSI <- loadOverture
          runtimeRes <- preloadedHSI stmts
          case runtimeRes of
            Left err -> hPutStrLn stderr ("Runtime Error! " ++ show err) >> exitFailure
            Right _ -> do
              return ()
    (Bad msg) -> hPutStrLn stderr msg >> exitFailure

parseFile :: String -> IO ()
parseFile filename = readFile filename >>= parse


main :: IO ()
main = do
  files <- getArgs
  mapM_ parseFile files
