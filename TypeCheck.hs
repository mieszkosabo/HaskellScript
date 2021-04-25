module TypeCheck where
import Data.Map(Map, empty, lookup, insert)
import Data.Maybe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe(isNothing)
import AbsHaskellScript
import Types
import Substitutions(substitute)
import TypeCheckUtils

-- here we can assume that the arity is valid
determineTypesAfterApp :: [Type] -> [Expr] -> TypeCheck Type
determineTypesAfterApp types args = do
  givenTypes <- mapM evalType args
  determinedTypes <- liftIO $ substitute types givenTypes
  case determinedTypes of
    Left err -> throwError err
    Right (types', _) -> 
      if length args < length types - 1 then
        return $ FunT $ drop (length args) types'
      else return $ last types'

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

-- FIXME: jakieś sprawdzanie by się przydało w sumie, ale jeszce nie wiem jak duże
evalType (ConciseLambda idents _) =
  return $ FunT $ replicate (length idents + 1) (WildcardT $ Ident lambdaWildcard)

evalType (LongLambda idents _) = 
  return $ FunT $ replicate (length idents + 1) (WildcardT $ Ident lambdaWildcard)

-- TODO: jak wymusić używanie spreada tylko wewnątrz list?
evalType (Spread e) = do
  t <- evalType e
  case t of
    ListT elType -> return elType
    _ -> throwError SpreadAppliedNotToList

evalType (ListExpr exprs) = do
  ts <- mapM evalType exprs
  if checkIfhomogeneousTypes (filter (/= Void) ts) then
    return $ ListT (if null ts then Void else head ts)
  else throwError HeterogenousList

evalType (EApp e args) = do
  (FunT argTypes) <- evalType e
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
      if isEqType t1 && isEqType t2 && compareType t1 t2 then
        return Bool
      else throwError $ TypeAssertFailed (show t1) (show t2)
    _ -> do
      ts <- mapM evalType [e, e']
      mapM_ (assertType Int) ts
      return Bool

-- evalType e = do
--   liftIO $ print e
--   return Void
-- stmts

typeCheckCase :: Case -> Type -> TypeCheck Type
typeCheckCase (Case (ListExpr []) (Block stmts)) _ = do
  (_, retType) <- typeCheckStmts stmts
  return $ fromMaybe Void retType
typeCheckCase (Case (ListExpr [EVar (Ident x), Spread (EVar (Ident xs))]) (Block stmts)) (ListT t) = do
  env' <- addArgsTypesToEnv [x, xs] [t, ListT t]
  (_, retType) <- local (const env') (typeCheckStmts stmts)
  return $ fromMaybe Void retType
typeCheckCase (Case (EApp (EConstr (Udent name)) args) (Block stmts)) _ = do
  (FunT types) <- askType name
  names <- argsToStrings args
  env' <- addArgsTypesToEnv names types
  (_, retType) <- local (const env') (typeCheckStmts stmts)
  return $ fromMaybe Void retType

argsToStrings :: [Expr] -> TypeCheck [String]
argsToStrings = mapM f
    where 
      f :: Expr -> TypeCheck String
      f (EVar (Ident n)) = return n
      f _ = throwError $ FunctionApplicationError "Error in Case"

typeCheck :: Stmt -> TypeCheck (TEnv, ReturnedType)

typeCheck (Match (Ident ident) cases) = do
  identType <- askType ident
  env <- ask
  evaledCases <- mapM (`typeCheckCase` identType) cases
  assert (checkIfhomogeneousTypes evaledCases) "different return types of cases in match statement!"
  return (env, Just $ head evaledCases)

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
    _ -> do -- FIXME: make it more safe
      t <- evalType lambda
      return (insert name t env, Nothing)


typeCheck (DataDecl (Udent name) params constructors) = do
  let dt = DataType (Udent name) params
  env <- addConstructorsToEnv constructors dt
  return (env, Nothing)

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

runTypeCheck stmts = runExceptT $ runReaderT (typeCheckStmts stmts) empty

runPreloadedTypeCheck env stmts = runExceptT $ runReaderT (typeCheckStmts stmts) env
