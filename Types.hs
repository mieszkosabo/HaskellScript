module Types where

import Data.Map ( Map )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.State ( StateT )
import Control.Monad.Except ( ExceptT )
import AbsHaskellScript


type VarName = String
type Loc = Integer

-- function body, names of the args, closure env
type FuncDef = ([Stmt], [Ident], Env)
type ListDef = [Value]
-- type AlgType = (Udent, [Type])
type Constr = Udent
type DataDef = (Constr, [Value])

data Value = IntVal Integer
           | BoolVal Bool
           | StringVal String
           | VoidVal
          | ListVal ListDef
          | FuncVal FuncDef
          | DataVal DataDef
          | ConstrVal Constr [TypeArg]

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
  show (ConstrVal udent args) = show udent ++ show args
  -- special printing for built-in list data type
  show (DataVal (Udent "EmptyList_", _)) = "[]"
  show (DataVal (Udent "L_", [x, rest])) = show $ f x ++ f rest
    where
      f :: Value -> [Value]
      f (DataVal (Udent "L_", [x', rest'])) = f x' ++ f rest'
      f (DataVal (Udent "EmptyList_", _)) = []
      f x = [x]
  show (DataVal (constr, values)) = show constr ++ show values

type ReturnedValue = Maybe Value

type Env = Map VarName Loc
type Store = Map Loc Value

data RunTimeErrors = DivisionByZeroException
                   | ModByZeroException
  deriving Show

data TypeCheckErrors = UndefinedName String
                     | TypeAssertFailed String String
                     | ReturnTypeVary String String
                     | SpreadAppliedNotToList
                     | HeterogenousList
                     | FunctionApplicationError String
                     | AssertionError String
                     | ReassignError String
  deriving Show

type TEnv = Map VarName Type
type TypeCheck = ReaderT TEnv (ExceptT TypeCheckErrors IO)
type ReturnedType = Maybe Type

lambdaWildcard = "λ"
-- HaskellScript Interpreter
type HSI = ReaderT Env (StateT Store (ExceptT RunTimeErrors IO))
