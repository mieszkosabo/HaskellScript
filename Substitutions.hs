module Substitutions where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.State
import Data.Map( Map, empty, lookup, insert )
import AbsHaskellScript
import Types
import TypeCheckUtils ( compareType, typeArgsToTypes, typesToTypeArgs )

type Substitutions = Map String Type
type SubsMonad = StateT Substitutions (ExceptT TypeCheckErrors IO) 

substitute :: [Type] -> [Type] -> IO (Either TypeCheckErrors ([Type], Substitutions))
substitute ts ts' = do runExceptT $ runStateT (substituteAll ts ts') empty

substituteAll :: [Type] -> [Type] -> SubsMonad [Type]
substituteAll ts ts' = do
  mapM_ (uncurry inferSubs_) (zip ts ts')
  mapM substitute_ ts

inferSubs_ :: Type -> Type -> SubsMonad Type
inferSubs_ (DataType pos (Udent name) ts) (DataType _ (Udent name') ts') = do
  if name /= name' then do
    throwError $ FunctionApplicationError pos "DataType udents don't match"
  else do
    nts <- zipWithM inferSubs_ (typeArgsToTypes ts) (typeArgsToTypes ts')
    return $ DataType Nothing (Udent name) $ typesToTypeArgs nts
inferSubs_ (ListT _ t) (ListT _ (Void _)) = return t
inferSubs_ (ListT _ (Void _)) (ListT _ t) = return t
inferSubs_ (ListT _ t) (ListT _ t') = do
  nt <- inferSubs_ t t'
  return $ ListT Nothing nt
inferSubs_ (FunT _ ts) (FunT _ ts') = do
  ts'' <- zipWithM inferSubs_ ts ts'
  return $ FunT Nothing ts''
inferSubs_ (WildcardT pos (Ident ident)) t = do
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
                else throwError $ FunctionApplicationError pos $ "sth with wildcards" ++ (show t) ++ " oraz" ++ (show t')

inferSubs_ t wildcard@(WildcardT _ (Ident ident)) = do
  if ident == lambdaWildcard then
    return t
  else inferSubs_ t wildcard
-- Int, Str, Bool case
inferSubs_ t t' = if compareType t t' then
                      return t
                    else do
                      liftIO $ print t >> print t'
                      throwError $ FunctionApplicationError Nothing "int str bool case"

substitute_ :: Type -> SubsMonad Type
substitute_ (DataType pos udent ts) = do
  ts' <- mapM substitute_ $ typeArgsToTypes ts
  return $ DataType pos udent $ typesToTypeArgs ts'
substitute_ (ListT pos t) = do
  t' <- substitute_ t
  return $ ListT pos t'
substitute_ (FunT pos ts) = do
  ts' <- mapM substitute_ ts
  return $ FunT pos ts'
substitute_ (WildcardT pos (Ident ident)) = do
  subs <- get
  case Data.Map.lookup ident subs of
    Nothing -> return (WildcardT pos (Ident ident))
    Just t' -> return t'
substitute_ t = return t
