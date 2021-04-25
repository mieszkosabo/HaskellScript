module Substitutions where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.State
import Data.Map( Map, empty, lookup, insert )
import AbsHaskellScript
import Types
import TypeCheckUtils ( compareType )

type Substitutions = Map String Type
type SubsMonad = StateT Substitutions (ExceptT TypeCheckErrors IO) 

substitute :: [Type] -> [Type] -> IO (Either TypeCheckErrors ([Type], Substitutions))
substitute ts ts' = do runExceptT $ runStateT (substituteAll ts ts') empty

substituteAll :: [Type] -> [Type] -> SubsMonad [Type]
substituteAll ts ts' = do
  mapM_ (uncurry inferSubs_) (zip ts ts')
  mapM substitute_ ts

inferSubs_ :: Type -> Type -> SubsMonad Type
inferSubs_ (DataType (Udent name) ts) (DataType (Udent name') ts') = do
  if name /= name' then do
    throwError $ FunctionApplicationError "DataType udents don't match"
  else do
    nts <- zipWithM inferSubs_ ts ts'
    return $ DataType (Udent name) nts
inferSubs_ (ListT t) (ListT t') = do
  nt <- inferSubs_ t t'
  return $ ListT nt
inferSubs_ (FunT ts) (FunT ts') = do
  ts'' <- zipWithM inferSubs_ ts ts'
  return $ FunT ts''
inferSubs_ (WildcardT (Ident ident)) t = do
  if ident == lambdaWildcard then
    return t
  else do
    subs <- get
    case Data.Map.lookup ident subs of
      Nothing -> do
        modify (insert ident t)
        return t
      Just t' -> if compareType t t' then
                  return t
                else throwError $ FunctionApplicationError $ "sth with wildcards" ++ (show t) ++ " oraz" ++ (show t')
-- Int, Str, Bool case
inferSubs_ t t' = if t == t' then
                      return t
                    else throwError $ FunctionApplicationError "int str bool case"

substitute_ :: Type -> SubsMonad Type
substitute_ (DataType udent ts) = do
  ts' <- mapM substitute_ ts
  return $ DataType udent ts'
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
substitute_ t = return t
