module Types where

import Data.Map ( Map )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.State ( StateT )
import Control.Monad.Except ( ExceptT )


type VarName = String
type Loc = Integer
data Value = IntVal Integer
           | BoolVal Bool
           | StringVal String
           | VoidVal
--           | ListVal ListDef
--           | FuncVal FuncDef
  deriving Show

type ReturnedValue = Maybe Value

type Env = Map VarName Loc
type Store = Map Loc Value

data RunTimeErrors = DivisionByZeroException
  deriving Show

-- HaskellScript Interpreter
type HSI = ReaderT Env (StateT Store (ExceptT RunTimeErrors IO))