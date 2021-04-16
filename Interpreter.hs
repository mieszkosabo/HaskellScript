module Interpreter where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Map(empty, insert, toDescList, lookup)
import Data.Maybe
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


eval :: Expr -> HSI Value
-- TODO: EApp, LApp, Lambda, List
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
  return $ IntVal $ evalMulOp op n n'

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

eval (ERel e op e') = do
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

execStmt (SExp _) = continueExec

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


execStmts :: [Stmt] -> HSI (Env, ReturnedValue)
execStmts [] = continueExec
execStmts (s:rest) = do
  (env, ret) <- execStmt s
  if isNothing ret then
    local (const env) (execStmts rest)
  else
    return (env, ret)


runHSI stmts = runExceptT $ runStateT (runReaderT (execStmts stmts) empty) empty