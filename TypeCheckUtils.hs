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
isEqType Int = True
isEqType Str = True
isEqType Bool = True
isEqType Void = True
isEqType (ListT t) = isEqType t
isEqType (WildcardT _) = True
isEqType _ = False

askType :: String -> TypeCheck Type
askType name = do
  env <- ask
  let maybeType = Data.Map.lookup name env
  case maybeType of
    Just t -> return t
    Nothing -> throwError $ UndefinedName name

assertType :: Type -> Type -> TypeCheck ()
-- for now, polimorphic arguments could be dangerous
assertType _ (WildcardT _) = return ()
assertType t t' = do
  if compareType t t' then
    return ()
  else throwError $ TypeAssertFailed (show t) (show t')

assert :: Bool -> String -> TypeCheck ()
assert True _ = return ()
assert False msg = throwError $ AssertionError msg 

continueTypeCheck :: TypeCheck (TEnv, ReturnedType)
continueTypeCheck = do
  env <- ask
  return (env, Nothing)

assertArgCount :: [Type] -> [Expr] -> TypeCheck ()
assertArgCount types exprs = do
  when
    ((length types == 1 && not (null exprs))
       || ((length types - 1) < length exprs))
    $ throwError $ FunctionApplicationError $ "Argument count assertion failed! " ++ show (length types) ++ show (length exprs)

declareVarType :: String -> Type -> TypeCheck TEnv
declareVarType n t = do
  env <- ask
  case Data.Map.lookup n env of
    (Just v) -> do
      liftIO $ print v
      throwError $ ReassignError n
    Nothing -> do
    let env' = insert n t env
    return env'

addArgsTypesToEnv :: [String] -> [Type] -> TypeCheck TEnv
addArgsTypesToEnv [] [] = ask
addArgsTypesToEnv names types = do
  env <- ask
  foldM f env (zip names types)
  where
    f = \env (name, typ) -> local (const env) $ declareVarType name typ

addConstructorsToEnv :: [Constructor] -> Type -> TypeCheck TEnv
addConstructorsToEnv [] _ = ask
addConstructorsToEnv constructors dt = do
  env <- ask
  foldM f env constructors
  where
    f = \env (Constructor (Udent name) typeargs) -> local (const env) $ declareVarType name (FunT $ typeArgsToTypes typeargs ++ [dt])

typeArgsToTypes :: [TypeArg] -> [Type]
typeArgsToTypes = map (\(TypeArg t) -> t)

typesToTypeArgs :: [Type] -> [TypeArg]
typesToTypeArgs = map TypeArg

compareType :: Type -> Type -> Bool
compareType (WildcardT _) _ = True
compareType _ (WildcardT _) = True
compareType (ListT _) (ListT Void) = True
compareType (ListT Void) (ListT _) = True
compareType t t' = t == t'

