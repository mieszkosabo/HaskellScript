module TypeCheck where
import Data.Map(Map, empty, lookup, insert)
import Data.Maybe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe(isNothing)
import AbsHaskellScript
import Types


type TEnv = Map VarName Type
type TypeCheck = ReaderT TEnv (ExceptT TypeCheckErrors IO)
type ReturnedType = Maybe Type

-- utils
sameVals :: (Eq a) => [a] -> Bool
sameVals [] = True
sameVals (x:xs) = all (== x) xs

isEqType :: Type -> Bool
isEqType Int = True
isEqType Str = True
isEqType Bool = True
isEqType (ListT t) = isEqType t

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
  if t == t' then
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
    $ throwError FunctionApplicationError

declareVarType :: String -> Type -> TypeCheck TEnv
declareVarType n t = do
  env <- ask
  let env' = insert n t env
  return env'

addArgsTypesToEnv :: [String] -> [Type] -> TypeCheck TEnv
addArgsTypesToEnv [] [] = ask
addArgsTypesToEnv names types = do
  env <- ask
  foldM f env (zip names types)
  where
    f = \env (name, typ) -> local (const env) $ declareVarType name typ

compareType :: Type -> Type -> Bool
compareType (WildcardT _) _ = True
compareType _ (WildcardT _) = True
compareType t t' = t ==t'

-- here we can assume that the arity is valid
determineTypesAfterApp :: [Type] -> [Expr] -> TypeCheck Type
determineTypesAfterApp types args = do
  givenTypes <- mapM evalType args
  determinedTypes <- determineTypes types givenTypes
  liftIO $ print determinedTypes
  return $  if length args < length types - 1 then
                FunT $ drop (length args) determinedTypes
              else
                  last determinedTypes

determineTypes :: [Type] -> [Type] -> TypeCheck [Type]
determineTypes types argsTypes = do
  let substitutions = foldl f (Just Data.Map.empty) (zip types argsTypes)
  case substitutions of 
    Nothing -> throwError FunctionApplicationError
    Just subs -> do
      return $ map (\t -> fromMaybe t (Data.Map.lookup t subs)) types
  where
    f Nothing _ = Nothing
    f (Just subs) (t, argT) = case Data.Map.lookup t subs of
      Nothing -> Just $ Data.Map.insert t argT subs
      Just someT -> if compareType someT argT then
                      Just subs
                    else
                      Nothing

-- exprs

evalType :: Expr -> TypeCheck Type
evalType (EVar (Ident name)) = askType name
evalType (EConstr (Udent name)) = askType name
evalType (ELitInt _) = return Int
evalType (EString _) = return Str
evalType ELitTrue = return Bool
evalType ELitFalse = return Bool
evalType (Ternary e1 e2 e3) = do
  t1 <- evalType e1
  assertType Bool t1
  t2 <- evalType e2
  t3 <- evalType e3
  if t2 == t3 then
    return t2
  else throwError $ ReturnTypeVary (show t2) (show t3)

--  FIXME: todo lambdas

-- TODO: jak wymusić używanie spreada tylko wewnątrz list?
evalType (Spread e) = do
  t <- evalType e
  case t of
    ListT elType -> return elType
    _ -> throwError SpreadAppliedNotToList

evalType (ListExpr exprs) = do
  ts <- mapM evalType exprs
  if sameVals ts then
    return $ ListT (if null ts then Void else head ts)
  else throwError HeterogenousList

evalType (EApp e args) = do
  (FunT argTypes) <- evalType e
  -- FIXME: tu może też być DataType chyba
  --liftIO $ print argTypes
  --liftIO $ print args
  assertArgCount argTypes args
  determineTypesAfterApp argTypes args

evalType (Neg e) = do
  t <- evalType e
  assertType Int t
  return Int
evalType (Not e) = do
  t <- evalType e
  assertType Bool t
  return Bool
evalType (EMul e _ e') = do
  ts <- mapM evalType [e, e']
  mapM_ (assertType Int) ts
  return Int
evalType (EAdd e _ e') = do
  ts <- mapM evalType [e, e']
  mapM_ (assertType Int) ts
  return Int
evalType (EAnd e e') = do
  ts <- mapM evalType [e, e']
  mapM_ (assertType Bool) ts
  return Bool
evalType (EOr e e') = do
  ts <- mapM evalType [e, e']
  mapM_ (assertType Bool) ts
  return Bool

-- TODO: eq for data types
evalType (ERel e op e') = do
  t1 <- evalType e
  t2 <- evalType e'
  case op of
    EQU -> do
      if isEqType t1 && isEqType t2 && t1 == t2 then
        return Bool
      else throwError $ TypeAssertFailed (show t1) (show t2)
    _ -> do
      ts <- mapM evalType [e, e']
      mapM_ (assertType Int) ts
      return Bool
-- stmts

typeCheck :: Stmt -> TypeCheck (TEnv, ReturnedType)
typeCheck (Print exprs) = do
  mapM_ evalType exprs
  env <- ask
  return (env, Nothing)

typeCheck (Decl (Ident name) e) = do
  t <- evalType e
  env' <- declareVarType name t
  return (env', Nothing)

typeCheck (FunDecl (Ident name) argTypes (Ident name') lambda) = do
  assert (name == name') $ "Declaration names don't match for: " ++ name ++ " and " ++ name'
  env <- ask
  case lambda of
    (ConciseLambda idents lexpr) -> do
      assert (length argTypes - 1 == length idents) $ "number of arguments don't match in signature and definition of" ++ name
      env' <- addArgsTypesToEnv (map (\(Ident n) -> n) idents) argTypes
      let env'' = insert name (FunT argTypes) env'
      retType <- local (const env'') (evalType lexpr)
      assertType retType (last argTypes)
      return (insert name (FunT argTypes) env, Nothing)
    (LongLambda idents (Block stmts)) -> do
      assert (length argTypes - 1 == length idents) $ "number of arguments don't match in signature and definition of" ++ name
      env' <- addArgsTypesToEnv (map (\(Ident n) -> n) idents) argTypes
      let env'' = insert name (FunT argTypes) env'
      (_, retType) <- local (const env'') (typeCheckStmts stmts)
      assertType (fromMaybe Void retType) (last argTypes)
      return (insert name (FunT argTypes) env, Nothing)


-- TODO: datadecl

typeCheck (Cond e (Block stmts)) = do
  t <- evalType e
  assertType t Bool
  typeCheckStmts stmts

typeCheck (CondElse e (Block stmts) (Block stmts')) = do
  t <- evalType e
  assertType t Bool
  (_, ret) <- typeCheckStmts stmts
  (_, ret') <- typeCheckStmts stmts'
  case (ret, ret') of
    (Nothing, Nothing) -> continueTypeCheck
    (Just t1, Just t2) -> 
      (if compareType t1 t2 then
       (do currEnv <- ask
           return (currEnv, ret))
      else
        throwError $ ReturnTypeVary (show t1) (show t2))
    (_, _) -> throwError $ ReturnTypeVary (show ret) (show ret') 

typeCheck (Ret e) = do
  t <- evalType e
  env <- ask
  return (env, Just t)

typeCheck VoidRet = do
  env <- ask
  return (env, Just Void)

-- TODO: match

typeCheck (SExp e) = do
  evalType e
  env <- ask
  return (env, Nothing)
        

typeCheckStmts :: [Stmt] -> TypeCheck (TEnv, ReturnedType)
typeCheckStmts [] = continueTypeCheck
typeCheckStmts (s:rest) = do
  (env, ret) <- typeCheck s
  if isNothing ret then
    local (const env) (typeCheckStmts rest)
  else
    return (env, ret)

runTypeCheck prog = runExceptT $ runReaderT (typeCheckStmts prog) empty