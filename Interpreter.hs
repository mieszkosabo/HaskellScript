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

varToLoc :: String -> HSI Loc
varToLoc name = do
  env <- ask
  --liftIO $ print name
  let Just loc = Data.Map.lookup name env
  return loc

declareVar :: String -> Value -> HSI Env
declareVar name value = do
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

varToValue :: String -> HSI Value
varToValue name = do
  loc <- varToLoc name
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
checkEq _ _ = False

addArgsToEnv :: [String] -> [Value] -> HSI Env
addArgsToEnv names values = do
  env <- ask
  foldM f env (zip names values)
  where
    f = \env (ident, value) -> local (const env) $ declareVar ident value

eval :: Expr -> HSI Value

eval (ConciseLambda args e) = do
  env <- ask
  return $ FuncVal ([Ret e], args, env)

eval (LongLambda args (Block stmts)) = do
  env <- ask
  return $ FuncVal (stmts, args, env) 

eval (EConstr (Udent s)) = varToValue s

eval (EApp e args) = do
  callabeValue <- eval e
  case callabeValue of
    FuncVal (stmts, idents, env) -> do
      env' <- ask
      values <- mapM eval args
      let names = map (\(Ident name) -> name) idents
      env <- local (const env) (addArgsToEnv names values)
      let isPartial = length args < length idents
      (if isPartial then
          return $ FuncVal (stmts, drop (length args) idents, env)
      else
          (do (_, ret) <- local (const $ union env env') $ execStmts stmts
              case ret of
                Nothing -> return VoidVal
                Just val -> return val))
    
    (ConstrVal udent types) -> do
      values <- mapM eval args
      -- TODO: errors
      return $ DataVal (udent, values)

eval (ListExpr expressions) =
  foldM f (DataVal (Udent "EmptyList_", [])) (reverse expressions)
  where
    f vals (Spread e) = do
      r <- eval e
      let values = g r
      return $ foldr (\val acc -> DataVal (Udent "L_", [val, acc])) vals values
        where 
          g (DataVal (_, [])) = []
          g (DataVal (_, [x, rest])) = x:g rest
    f vals e = do
      value <- eval e
      return $ DataVal (Udent "L_", [value, vals])

eval (EVar (Ident name)) = varToValue name
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

execStmt (Decl (Ident name) e) = do
  value <- eval e
  env' <- declareVar name value
  return (env', Nothing)

execStmt (FunDecl _ _ (Ident name) e) = do
  value <- eval e
  env' <- declareVar name value
  return (env', Nothing)

execStmt (DataDecl udent params constructors) = do
  let constrValues = map (\(Constructor udent types) -> ConstrVal udent types) constructors
  let udents = map (\(Constructor udent types) -> udent) constructors
  env <- ask
  let names = map (\(Udent name) -> name) udents
  env' <- local (const env) (addArgsToEnv names constrValues)
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

execStmt (Match (Ident name) cases) = do
  value <- varToValue name
  let maybeMatchingCase = find (doesCaseMatch value) cases
  case maybeMatchingCase of
    Nothing -> continueExec
    Just matchingCase -> runCase matchingCase value


runCase :: Case -> Value -> HSI (Env, ReturnedValue) 
runCase (Case (ListExpr []) (Block stmts)) _ = do
  execStmts stmts
runCase (Case (ListExpr [EVar (Ident x), Spread (EVar (Ident xs))]) (Block stmts)) (DataVal (_, [x', xs'])) = do
  env <- ask
  env' <- local (const env) $ declareVar x x'
  env' <- local (const env') $ declareVar xs xs' 
  (_, ret) <- local (const env') $ execStmts stmts
  return (env, ret)

runCase (Case (EApp _ exprs) (Block stmts)) (DataVal (_, vals)) = do
  env <- ask
  let names = map (\(EVar (Ident name)) -> name) exprs
  env' <- local (const env) $ addArgsToEnv names vals
  (_, ret) <- local (const env') $ execStmts stmts
  return (env, ret)

doesCaseMatch :: Value -> Case -> Bool
-- syntax sugar for lists
doesCaseMatch  (DataVal (Udent "EmptyList_", _)) (Case (ListExpr []) _) = True
doesCaseMatch  (DataVal (Udent "L_", _)) (Case (ListExpr [EVar _, Spread _]) _) = True

doesCaseMatch (DataVal (udent, vals)) (Case (EApp (EConstr udent') exprs) _) =
  udent == udent' && length vals == length exprs
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