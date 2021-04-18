module Types where

import Data.Map ( Map )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.State ( StateT )
import Control.Monad.Except ( ExceptT )
import AbsHaskellScript ( Stmt, Ident)


type VarName = String
type Loc = Integer

-- function body, names of the args, closure env
type FuncDef = ([Stmt], [Ident], Env)

type ListDef = [Value]
data Value = IntVal Integer
           | BoolVal Bool
           | StringVal String
           | VoidVal
          | ListVal ListDef
          | FuncVal FuncDef

instance Show Value where
  show (IntVal n) = show n
  show (BoolVal True) = "true"
  show (BoolVal False) = "false"
  show (StringVal s) = s
  show VoidVal = "undefined"
  show (FuncVal (stmts, args, env)) = 
    show args ++ "\n"
    ++ show stmts ++ "\n"
    ++ show env
  show (ListVal vs) = show vs

type ReturnedValue = Maybe Value

type Env = Map VarName Loc
type Store = Map Loc Value

data RunTimeErrors = DivisionByZeroException
                   | ModByZeroException
                   
  deriving Show

-- HaskellScript Interpreter
type HSI = ReaderT Env (StateT Store (ExceptT RunTimeErrors IO))