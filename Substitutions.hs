module Substitutions where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Control.Monad.Except
import Control.Monad.State
import Data.Map(Map, empty, lookup, insert)
import AbsHaskellScript
import Types

type Substitutions = Map String Type
type SubsMonad = StateT Substitutions (ExceptT TypeCheckErrors IO) 

substitute :: [Type] -> [Type] -> IO (Either TypeCheckErrors ([Type], Substitutions))
substitute ts ts' = do runExceptT $ runStateT (substituteAll ts ts') empty

substituteAll :: [Type] -> [Type] -> SubsMonad [Type]
substituteAll ts ts' = do
  mapM_ (uncurry inferSubs_) (zip ts ts')
  mapM substitute_ ts

inferSubs_ :: Type -> Type -> SubsMonad Type
inferSubs_ (ListT t) (ListT t') = do
  nt <- inferSubs_ t t'
  return $ ListT nt
inferSubs_ (FunT ts) (FunT ts') = do
  ts'' <- zipWithM inferSubs_ ts ts'
  return $ FunT ts''
inferSubs_ (WildcardT (Ident ident)) t = do
  subs <- get
  case Data.Map.lookup ident subs of
    Nothing -> do
      modify (insert ident t)
      return t
    Just t' -> if t == t' then
                return t
               else throwError FunctionApplicationError
-- Int, Str, Bool case
inferSubs_ t t' = if t == t' then
                      return t
                    else throwError FunctionApplicationError

substitute_ :: Type -> SubsMonad Type
substitute_ Int = return Int
substitute_ Str = return Str
substitute_ Bool = return Bool
substitute_ Void = return Void
substitute_ (ListT t) = do
  t' <- substitute_ t
  return $ ListT t'
substitute_ (FunT ts) = do
  ts' <- mapM substitute_ ts
  return $ FunT ts'
substitute_ (WildcardT (Ident ident)) = do
  subs <- get
  case Data.Map.lookup ident subs of
    Nothing -> return (WildcardT (Ident ident))
    Just t' -> return t'

-- FIXME: inferSubs_ _ _ = throwError