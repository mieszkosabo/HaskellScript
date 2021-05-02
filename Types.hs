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

data RunTimeErrors = DivisionByZeroException BNFC'Position
                   | ModByZeroException BNFC'Position

instance Show RunTimeErrors where
  show (DivisionByZeroException pos) = "Division by zero" ++ addPositionInfo pos
  show (ModByZeroException pos) = "Zero was used as a second argument to modulo" ++ addPositionInfo pos

data TypeCheckErrors = UndefinedName BNFC'Position String
                     | TypeAssertFailed BNFC'Position String String
                     | ReturnTypeVary BNFC'Position String String
                     | SpreadAppliedNotToList BNFC'Position
                     | HeterogenousList BNFC'Position
                     | FunctionApplicationError BNFC'Position String
                     | AssertionError BNFC'Position String
                     | ReassignError BNFC'Position String

addPositionInfo :: BNFC'Position -> String
addPositionInfo (Just (line, col)) = " near line " ++ show line ++ ", column " ++ show col ++ "."
addPositionInfo Nothing = "."
instance Show TypeCheckErrors where
  show (UndefinedName pos name) = "Undefined name: " ++ name ++ addPositionInfo pos
  show (TypeAssertFailed pos t1 t2) = "Expected type: " ++ t2 ++ ", but got: " ++ t1 ++ addPositionInfo pos
  show (ReturnTypeVary pos t1 t2) = "Unexpected return type. Expected: " ++ t1 ++ ", but got: " ++ t2 ++ addPositionInfo pos
  show (SpreadAppliedNotToList pos) = "Spread operator was applied to a different type than a List" ++ addPositionInfo pos
  show (HeterogenousList pos) = "Lists cannot be heterogenous" ++ addPositionInfo pos
  show (FunctionApplicationError pos msg) = "Error in function application: " ++ msg ++ addPositionInfo pos
  show (AssertionError pos msg) = msg ++ addPositionInfo pos
  show (ReassignError pos name) = "Attempt to reassign a constant: " ++ name ++ addPositionInfo pos
type TEnv = Map VarName Type
type TypeCheck = ReaderT TEnv (ExceptT TypeCheckErrors IO)
type ReturnedType = Maybe Type

lambdaWildcard = "_λ" -- reserved type name for lambdas without type signature
-- HaskellScript Interpreter
type HSI = ReaderT Env (StateT Store (ExceptT RunTimeErrors IO))
