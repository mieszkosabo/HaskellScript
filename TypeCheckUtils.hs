module TypeCheckUtils where

import Types
import AbsHaskellScript
import Control.Monad.Except
import Data.Map(lookup, insert)
import Control.Monad.Reader

checkIfhomogeneousTypes :: [Type] -> Bool
checkIfhomogeneousTypes [] = True
checkIfhomogeneousTypes (x:xs) = all (compareType x) xs

isEqType :: Type -> Bool
isEqType (Int _) = True
isEqType (Str _) = True
isEqType (Bool _) = True
isEqType (Void _) = True
isEqType (ListT _ t) = isEqType t
isEqType (WildcardT _ _) = True
isEqType _ = False

askType :: String -> BNFC'Position -> TypeCheck Type
askType name pos = do
  env <- ask
  let maybeType = Data.Map.lookup name env
  case maybeType of
    Just t -> return t
    Nothing -> throwError $ UndefinedName pos name

assertType :: Type -> Type -> BNFC'Position -> TypeCheck ()
-- for now, polimorphic arguments could be dangerous
assertType _ (WildcardT _  _) _ = return ()
assertType t t' pos = do
  if compareType t t' then
    return ()
  else throwError $ TypeAssertFailed pos (show t) (show t')

assert :: Bool -> BNFC'Position -> String -> TypeCheck ()
assert True _ _ = return ()
assert False pos msg = throwError $ AssertionError pos msg 

continueTypeCheck :: TypeCheck (TEnv, ReturnedType)
continueTypeCheck = do
  env <- ask
  return (env, Nothing)

assertArgCount :: [Type] -> [Expr] -> BNFC'Position -> TypeCheck ()
assertArgCount types exprs pos = do
  when
    ((length types == 1 && not (null exprs))
       || ((length types - 1) < length exprs))
    $ throwError $ FunctionApplicationError pos $ "Argument count assertion failed! " ++ show (length types) ++ show (length exprs)

declareVarType :: String -> Type -> BNFC'Position -> TypeCheck TEnv
declareVarType n t pos = do
  env <- ask
  case Data.Map.lookup n env of
    (Just _) -> do
      throwError $ ReassignError pos n
    Nothing -> do
    let env' = insert n t env
    return env'

addArgsTypesToEnv :: [String] -> [Type] -> BNFC'Position -> TypeCheck TEnv
addArgsTypesToEnv [] [] _ = ask
addArgsTypesToEnv names types pos = do
  env <- ask
  foldM f env (zip names types)
  where
    f = \env (name, typ) -> local (const env) $ declareVarType name typ pos

addConstructorsToEnv :: [Constructor] -> Type -> BNFC'Position -> TypeCheck TEnv
addConstructorsToEnv [] _ _ = ask
addConstructorsToEnv constructors dt pos = do
  env <- ask
  foldM f env constructors
  where
    f = \env (Constructor pos' (Udent name) typeargs) -> local (const env) $ declareVarType name (FunT pos' (typeArgsToTypes typeargs ++ [dt])) pos

typeArgsToTypes :: [TypeArg] -> [Type]
typeArgsToTypes = map (\(TypeArg _ t) -> t)

typesToTypeArgs :: [Type] -> [TypeArg]
typesToTypeArgs = map (TypeArg Nothing)

compareType :: Type -> Type -> Bool
compareType (WildcardT _ ident) (WildcardT _ ident') = ident == ident'
compareType (WildcardT _ _) _ = True
compareType _ (WildcardT _ _) = True
compareType (ListT _ _) (ListT _ (Void _)) = True
compareType (ListT _ (Void _)) (ListT _ _) = True
compareType (Int _) (Int _) = True
compareType (Str _) (Str _) = True
compareType (Bool _) (Bool _) = True
compareType (Void _) (Void _) = True
compareType (ListT _ t) (ListT _ t') = compareType t t'
compareType (FunT _ ts) (FunT _ ts') = foldr (\(t1, t2) acc -> compareType t1 t2 && acc) True (zip ts ts')

compareType (DataType _ udent ts) (DataType _ udent' ts') 
  = udent == udent' && foldr (\(t1, t2) acc -> compareType t1 t2 && acc) True (zip (typeArgsToTypes ts) (typeArgsToTypes ts'))
compareType _ _ = False

