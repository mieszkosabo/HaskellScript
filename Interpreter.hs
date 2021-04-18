module Interpreter where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Map(empty, insert, toDescList, lookup, union)
import Data.Maybe
import Data.List(find)
import AbsHaskellScript

import Types

-- storing Variables

newloc :: Store -> Loc
newloc store =
  case toDescList store of
    [] -> 0
    ((k,_):_) -> k + 1

identToLoc :: Ident -> HSI Loc
identToLoc (Ident name) = do
  env <- ask
  --liftIO $ print name
  let Just loc = Data.Map.lookup name env
  return loc

declareIdent :: Ident -> Value -> HSI Env
declareIdent (Ident name) value = do
  env <- ask
  store <- get
  let loc = newloc store
  let env' = insert name loc env
  put (insert loc value store)
  return env'

updateValue :: Ident -> Value -> HSI ()
updateValue (Ident name) value = do
  env <- ask
  store <- get
  let Just loc = Data.Map.lookup name env
  put (insert loc value store)
  return ()

identToValue :: Ident -> HSI Value
identToValue ident = do
  loc <- identToLoc ident
  store <- get
  let Just value = Data.Map.lookup loc store
  return value

-- Expressions

evalAddOp :: AddOp -> Integer -> Integer -> Integer
evalAddOp Plus = (+)
evalAddOp Minus = (-)

evalMulOp :: MulOp -> Integer -> Integer -> Integer
evalMulOp Div = div
evalMulOp Mod = mod
evalMulOp Times = (*)

evalRelOp :: RelOp -> Integer -> Integer -> Bool
evalRelOp LTH = (<)
evalRelOp LE = (<=)
evalRelOp GTH = (>)
evalRelOp GE = (<=)
evalRelOp EQU = (==)
evalRelOp NE = (/=)

checkEq :: Value -> Value -> Bool
checkEq (IntVal n) (IntVal n') = evalRelOp EQU n n'
checkEq (StringVal s) (StringVal s') = s == s'
checkEq (BoolVal b) (BoolVal b') = b == b'
checkEq (ListVal vs) (ListVal vs') = 
  (length vs == length vs')
    && foldr (\ (v, v') res -> checkEq v v' && res) True (zip vs vs')
checkEq _ _ = False

addArgsToEnv :: [Ident] -> [Value] -> HSI Env
addArgsToEnv idents values = do
  env <- ask
  foldM f env (zip idents values)
  where
    f = \env (ident, value) -> local (const env) $ declareIdent ident value


eval :: Expr -> HSI Value

eval (ConciseLambda args e) = do
  env <- ask
  return $ FuncVal ([Ret e], args, env)

eval (LongLambda args (Block stmts)) = do
  env <- ask
  return $ FuncVal (stmts, args, env) 

eval (EApp e args) = do
  FuncVal (stmts, idents, env) <- eval e
  env' <- ask
  values <- mapM eval args
  env <- local (const env) (addArgsToEnv idents values)
  let isPartial = length args < length idents
  (if isPartial then
       return $ FuncVal (stmts, drop (length args) idents, env)
   else
       (do (_, ret) <- local (const $ union env env') $ execStmts stmts
           case ret of
             Nothing -> return VoidVal
             Just val -> return val))
    

eval (ListExpr expressions) = do
  values <- foldM f [] (reverse expressions)
  return $ ListVal values
  where
    f vals (Spread e) = do
      ListVal values <- eval e
      return $ values ++ vals
    f vals e = do
      value <- eval e
      return $ value:vals

eval (EVar name) = identToValue name
eval (EString s) = return $ StringVal s

eval (ELitInt n) = return $ IntVal n

eval (Neg e) = do
  IntVal n <- eval e
  return $ IntVal $ -n

eval (EAdd e op e') = do
  IntVal n <- eval e
  IntVal n' <- eval e'
  return $ IntVal $ evalAddOp op n n'

eval (EMul e op e') = do
  IntVal n <- eval e
  IntVal n' <- eval e'
  case op of
    Div -> if n' == 0 then throwError DivisionByZeroException else return $ IntVal $ evalMulOp op n n'
    Mod -> if n' == 0 then throwError ModByZeroException else return $ IntVal $ evalMulOp op n n'
    _ -> return $ IntVal $ evalMulOp op n n'

eval ELitTrue = return $ BoolVal True

eval ELitFalse = return $ BoolVal False

eval (Not e) = do
  BoolVal b <- eval e
  return $ BoolVal $ not b

eval (EAnd e e') = do
  BoolVal b  <- eval e
  BoolVal b' <- eval e'
  return $ BoolVal $ b && b'

eval (EOr e e') = do
  BoolVal b  <- eval e
  BoolVal b' <- eval e'
  return $ BoolVal $ b || b'

eval (ERel e op e') =
  case op of
    EQU -> do 
      v <- eval e
      v' <- eval e'
      return $ BoolVal $ checkEq v v'
    _ -> do
    IntVal n <- eval e
    IntVal n' <- eval e'
    return $ BoolVal $ evalRelOp op n n'

eval (Ternary e e' e'') = do
  BoolVal b <- eval e
  if b then do
    eval e'
  else do
    eval e''

runEval exp = runExceptT $ runStateT (runReaderT (eval exp) empty) empty

-- Statements
continueExec :: HSI (Env, ReturnedValue)
continueExec = do
  env <- ask
  return (env, Nothing)

execStmt :: Stmt -> HSI (Env, ReturnedValue)

execStmt (Decl name e) = do
  value <- eval e
  env' <- declareIdent name value
  return (env', Nothing)

execStmt (FunDecl _ _ name e) = do
  -- todo check idents if equal
  -- todo typechecking
  value <- eval e
  env' <- declareIdent name value
  return (env', Nothing)

execStmt (SExp e) = do
  eval e -- potential side effects here, that's why needs to be evaluated
  continueExec

execStmt (Cond e (Block stmts)) = do
  BoolVal b <- eval e
  if b then
    execStmts stmts
  else
    continueExec

execStmt (CondElse e (Block stmts) (Block stmts')) = do
  BoolVal b <- eval e
  if b then
    execStmts stmts
  else
    execStmts stmts'

execStmt (Ret e) = do
  value <- eval e
  env <- ask
  return (env, Just value)

execStmt VoidRet = do
  env <- ask
  return (env, Just VoidVal)

execStmt (Print expressions) = do
  res <- mapM eval expressions
  liftIO $ mapM_ (putStr . (++ " ") . show) res
  liftIO $ putStrLn ""
  continueExec

execStmt (Match ident cases) = do
  value <- identToValue ident
  let maybeMatchingCase = find (doesCaseMatch value) cases
  case maybeMatchingCase of
    Nothing -> continueExec
    Just matchingCase -> runCase matchingCase value


runCase :: Case -> Value -> HSI (Env, ReturnedValue) 
runCase (Case (ListExpr []) (Block stmts)) _ = do
  execStmts stmts
runCase (Case (ListExpr [EVar x, Spread (EVar xs)]) (Block stmts)) (ListVal (x':xs')) = do
  env <- ask
  env' <- local (const env) $ declareIdent x x'
  env' <- local (const env') $ declareIdent xs (ListVal xs')
  (_, ret) <- local (const env') $ execStmts stmts
  return (env, ret)


doesCaseMatch :: Value -> Case -> Bool
doesCaseMatch  (ListVal []) (Case (ListExpr []) _) = True
doesCaseMatch  (ListVal (_:_)) (Case (ListExpr [EVar _, Spread _]) _) = True
doesCaseMatch _ _ = False



execStmts :: [Stmt] -> HSI (Env, ReturnedValue)
execStmts [] = continueExec
execStmts (s:rest) = do
  (env, ret) <- execStmt s
  if isNothing ret then
    local (const env) (execStmts rest)
  else
    return (env, ret)


runHSI stmts = runExceptT $ runStateT (runReaderT (execStmts stmts) empty) empty

runPreloadedHSI env store stmts =runExceptT $ runStateT (runReaderT (execStmts stmts) env) store