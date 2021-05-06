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
evalAddOp (Plus _)  = (+)
evalAddOp (Minus _) = (-)

evalMulOp :: MulOp -> Integer -> Integer -> Integer
evalMulOp (Div _) = div
evalMulOp (Mod _) = mod
evalMulOp (Times _) = (*)

evalRelOp :: RelOp -> Integer -> Integer -> Bool
evalRelOp (LTH _) = (<)
evalRelOp (LE _)  = (<=)
evalRelOp (GTH _) = (>)
evalRelOp (GE _)  = (<=)
evalRelOp (EQU _) = (==)
evalRelOp (NE _)  = (/=)

checkEq :: Value -> Value -> Bool
checkEq (IntVal n) (IntVal n') = evalRelOp (EQU Nothing) n n'
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

eval (ConciseLambda pos args e) = do
  env <- ask
  return $ FuncVal ([Ret pos e], args, env)

eval (LongLambda _ args (Block _ stmts)) = do
  env <- ask
  return $ FuncVal (stmts, args, env) 

eval (EConstr _ (Udent s)) = varToValue s

eval (EApp pos e args) = do
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
    
    (ConstrVal udent _) -> do
      values <- mapM eval args
      return $ DataVal (udent, values)

    _ -> throwError $ InvalidTypeException pos 

eval (ListExpr _ expressions) =
  foldM f (DataVal (Udent "EmptyList_", [])) (reverse expressions)
  where
    f vals (Spread _ e) = do
      r <- eval e
      let values = g r
      return $ foldr (\val acc -> DataVal (Udent "L_", [val, acc])) vals values
        where 
          g (DataVal (_, [])) = []
          g (DataVal (_, [x, rest])) = x:g rest
    f vals e = do
      value <- eval e
      return $ DataVal (Udent "L_", [value, vals])

eval (EVar _ (Ident name)) = varToValue name
eval (EString _ s) = return $ StringVal s

eval (ELitInt _ n) = return $ IntVal n
-- If user declares a polymorphic function with some class (e.g. integer) specific operations in it, then
-- a runtime exception is possible as types aren't infered from the function body.
eval (Neg pos e) = do
  evaled <- eval e
  case evaled of 
    IntVal n -> return $ IntVal $ -n
    _ -> throwError $ InvalidTypeException pos
  

eval (EAdd pos e op e') = do
  evaled <- eval e
  evaled' <- eval e'
  case evaled of 
    IntVal n -> case evaled' of
      IntVal n' -> return $ IntVal $ evalAddOp op n n'
      _ -> throwError $ InvalidTypeException pos
    _ -> throwError $ InvalidTypeException pos
  

eval (EMul pos e op e') = do
  evaled <- eval e
  evaled' <- eval e'
  case evaled of 
    IntVal n -> case evaled' of
      IntVal n' -> case op of
        (Div pos) -> if n' == 0 then throwError $ DivisionByZeroException pos else return $ IntVal $ evalMulOp op n n'
        (Mod pos) -> if n' == 0 then throwError $ ModByZeroException pos else return $ IntVal $ evalMulOp op n n'
        _ -> return $ IntVal $ evalMulOp op n n'
      _ -> throwError $ InvalidTypeException pos
    _ -> throwError $ InvalidTypeException pos
  

eval (ELitTrue _) = return $ BoolVal True

eval (ELitFalse _) = return $ BoolVal False

eval (Not pos e) = do
  evaled <- eval e
  case evaled of 
    BoolVal b -> return $ BoolVal $ not b
    _ -> throwError $ InvalidTypeException pos
  

eval (EAnd pos e e') = do
  evaled <- eval e
  evaled' <- eval e'
  case evaled of 
    BoolVal b -> case evaled' of
      BoolVal b' -> return $ BoolVal $ b && b'
      _ -> throwError $ InvalidTypeException pos
    _ -> throwError $ InvalidTypeException pos

eval (EOr pos e e') = do
  evaled <- eval e
  evaled' <- eval e'
  case evaled of 
    BoolVal b -> case evaled' of
      BoolVal b' -> return $ BoolVal $ b || b'
      _ -> throwError $ InvalidTypeException pos
    _ -> throwError $ InvalidTypeException pos

eval (ERel pos e op e') =
  case op of
    (EQU _) -> do 
      v <- eval e
      v' <- eval e'
      return $ BoolVal $ checkEq v v'
    _ -> do
    evaled <- eval e
    evaled' <- eval e'
    case evaled of 
      IntVal n -> case evaled' of
        IntVal n' -> return $ BoolVal $ evalRelOp op n n'
        _ -> throwError $ InvalidTypeException pos
      _ -> throwError $ InvalidTypeException pos

eval (Ternary pos e e' e'') = do
  evaled <- eval e
  case evaled of 
    BoolVal b -> 
      if b then do
        eval e'
      else do
        eval e''
    _ -> throwError $ InvalidTypeException pos

runEval exp = runExceptT $ runStateT (runReaderT (eval exp) empty) empty

-- Statements
continueExec :: HSI (Env, ReturnedValue)
continueExec = do
  env <- ask
  return (env, Nothing)

execStmt :: Stmt -> HSI (Env, ReturnedValue)

execStmt (Decl _ (Ident name) e) = do
  value <- eval e
  env' <- declareVar name value
  return (env', Nothing)

execStmt (FunDecl _ _ _ (Ident name) e) = do
  value <- eval e
  env' <- declareVar name value
  return (env', Nothing)

execStmt (DataDecl _ udent params constructors) = do
  let constrValues = map (\(Constructor _ udent types) -> ConstrVal udent types) constructors
  let udents = map (\(Constructor _ udent types) -> udent) constructors
  env <- ask
  let names = map (\(Udent name) -> name) udents
  env' <- local (const env) (addArgsToEnv names constrValues)
  return (env', Nothing)

execStmt (SExp _ e) = do
  eval e -- potential side effects here, that's why needs to be evaluated
  continueExec

execStmt (Cond pos e (Block _ stmts)) = do
  evaled <- eval e
  case evaled of 
    BoolVal b ->
      if b then
        execStmts stmts
      else
        continueExec
    _ -> throwError $ InvalidTypeException pos 

execStmt (CondElse pos e (Block _ stmts) (Block _ stmts')) = do
  evaled <- eval e
  case evaled of
    BoolVal b ->
      if b then
        execStmts stmts
      else
        execStmts stmts'
    _ -> throwError $ InvalidTypeException pos

execStmt (Ret _ e) = do
  value <- eval e
  env <- ask
  return (env, Just value)

execStmt (VoidRet _) = do
  env <- ask
  return (env, Just VoidVal)

execStmt (Print _ expressions) = do
  res <- mapM eval expressions
  liftIO $ mapM_ (putStr . (++ " ") . show) res
  liftIO $ putStrLn ""
  continueExec

execStmt (Match _ (Ident name) cases) = do
  value <- varToValue name
  let maybeMatchingCase = find (doesCaseMatch value) cases
  case maybeMatchingCase of
    Nothing -> continueExec
    Just matchingCase -> runCase matchingCase value


runCase :: Case -> Value -> HSI (Env, ReturnedValue) 
runCase (Case _ (ListExpr _ []) (Block _ stmts)) _ = do
  execStmts stmts
runCase (Case _ (ListExpr _ [EVar _ (Ident x), Spread _ (EVar _ (Ident xs))]) (Block _ stmts)) (DataVal (_, [x', xs'])) = do
  env <- ask
  env' <- local (const env) $ declareVar x x'
  env' <- local (const env') $ declareVar xs xs' 
  (_, ret) <- local (const env') $ execStmts stmts
  return (env, ret)

runCase (Case _ (EApp _ _ exprs) (Block _ stmts)) (DataVal (_, vals)) = do
  env <- ask
  let names = map (\(EVar _ (Ident name)) -> name) exprs
  env' <- local (const env) $ addArgsToEnv names vals
  (_, ret) <- local (const env') $ execStmts stmts
  return (env, ret)

doesCaseMatch :: Value -> Case -> Bool
-- syntax sugar for lists
doesCaseMatch  (DataVal (Udent "EmptyList_", _)) (Case _ (ListExpr _ []) _) = True
doesCaseMatch  (DataVal (Udent "L_", _)) (Case _ (ListExpr _ [EVar _ _, Spread _ _]) _) = True

doesCaseMatch (DataVal (udent, vals)) (Case _ (EApp _ (EConstr _ udent') exprs) _) =
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