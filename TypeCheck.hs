module TypeCheck where
import Data.Map(empty, insert)
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Except
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
        return $ FunT Nothing $ drop (length args) types'
      else return $ last types'

-- exprs

evalType :: Expr -> TypeCheck Type
evalType (EVar pos (Ident name)) = askType name pos
evalType (EConstr pos (Udent name)) = askType name pos
evalType (ELitInt pos _) = return $ Int pos
evalType (EString pos _) = return $ Str pos
evalType (ELitTrue pos) = return  $ Bool pos
evalType (ELitFalse pos) = return $ Bool pos
evalType (Ternary pos e1 e2 e3) = do
  t1 <- evalType e1
  assertType (Bool Nothing) t1 pos
  t2 <- evalType e2
  t3 <- evalType e3
  if compareType t2 t3 then
    return t2
  else throwError $ ReturnTypeVary pos (show t2) (show t3)

evalType (ConciseLambda pos idents _) =
  return $ FunT pos $ replicate (length idents + 1) (WildcardT pos $ Ident lambdaWildcard)

evalType (LongLambda pos idents _) = 
  return $ FunT pos $ replicate (length idents + 1) (WildcardT pos $ Ident lambdaWildcard)

evalType (Spread pos e) = do
  t <- evalType e
  case t of
    ListT _ elType -> return elType
    _ -> throwError $ SpreadAppliedNotToList pos

evalType (ListExpr pos exprs) = do
  ts <- mapM evalType exprs
  if checkIfhomogeneousTypes (filter (/= Void Nothing) ts) then
    return $ ListT pos (if null ts then Void pos else head ts)
  else throwError $ HeterogenousList pos

evalType (EApp pos e args) = do
  (FunT _ argTypes) <- evalType e
  assertArgCount argTypes args pos
  determineTypesAfterApp argTypes args

evalType (Neg pos e) = do
  t <- evalType e
  assertType (Int Nothing) t pos
  return $ Int pos
evalType (Not pos e) = do
  t <- evalType e
  assertType (Bool Nothing) t pos
  return $ Bool pos
evalType (EMul pos e _ e') = do
  ts <- mapM evalType [e, e']
  mapM_ (\t -> assertType (Int Nothing) t pos) ts
  return $ Int pos
evalType (EAdd pos e _ e') = do
  ts <- mapM evalType [e, e']
  mapM_ (\t -> assertType (Int Nothing) t pos) ts
  return $ Int pos
evalType (EAnd pos e e') = do
  ts <- mapM evalType [e, e']
  mapM_ (\t -> assertType (Bool Nothing) t pos) ts
  return $ Bool pos
evalType (EOr pos e e') = do
  ts <- mapM evalType [e, e']
  mapM_ (\t -> assertType (Bool Nothing) t pos) ts
  return $ Bool pos

evalType (ERel pos e op e') = do
  t1 <- evalType e
  t2 <- evalType e'
  case op of
    EQU _ -> do
      if isEqType t1 && isEqType t2 && compareType t1 t2 then
        return $ Bool pos
      else throwError $ TypeAssertFailed pos (show t1) (show t2)
    _ -> do
      ts <- mapM evalType [e, e']
      mapM_ (\t -> assertType (Int Nothing) t pos) ts
      return $ Bool pos

typeCheckCase :: Case -> Type -> TypeCheck Type
typeCheckCase (Case pos expr (Block _ stmts)) t = do
  env' <- addPatternTypesToEnv t expr
  (_, retType) <- local (const env') (typeCheckStmts stmts)
  return $ fromMaybe (Void pos) retType

addPatternTypesToEnv :: Type -> Expr -> TypeCheck TEnv
addPatternTypesToEnv t (EVar pos (Ident x)) =
  if last x == '_' then do
    ask
  else do
    env <- ask
    local (const env) $ declareVarType x t pos
addPatternTypesToEnv (ListT pos t) (ListExpr _ [x, Spread _ y]) = do
  env <- ask
  env' <- local (const env) $ addPatternTypesToEnv t x
  local (const env') $ addPatternTypesToEnv (ListT pos t) y
addPatternTypesToEnv (DataType _ (Udent udent) _) (EApp pos (EConstr pos' (Udent constrName)) exprs) = do
  (FunT _ ts) <- askType constrName pos'
  if length ts - 1 /= length exprs then
    throwError $ PatternMatchingError pos $ "invalid number of arguments in pattern for " ++ udent
  else do
    env <- ask
    foldM f env (zip ts exprs)
    where
      f = \env (t, e) -> local (const env) $ addPatternTypesToEnv t e
addPatternTypesToEnv _ _ = do ask
      
argsToStrings :: [Expr] -> TypeCheck [String]
argsToStrings = mapM f
    where 
      f :: Expr -> TypeCheck String
      f (EVar _ (Ident n)) = return n
      f _ = throwError $ FunctionApplicationError Nothing "Error in Case"

typeCheck :: Stmt -> TypeCheck (TEnv, ReturnedType)

typeCheck (Match pos (Ident ident) cases) = do
  identType <- askType ident pos
  env <- ask
  evaledCases <- mapM (`typeCheckCase` identType) cases
  assert (checkIfhomogeneousTypes evaledCases) pos "different return types of cases in match statement!"
  return (env, Just $ head evaledCases)

typeCheck (Print _ exprs) = do
  mapM_ evalType exprs
  env <- ask
  return (env, Nothing)

typeCheck (Decl pos (Ident name) e) = do
  t <- evalType e
  env' <- declareVarType name t pos
  return (env', Nothing)

typeCheck (FunDecl pos (Ident name) argTypes (Ident name') lambda) = do
  assert (name == name') pos $ "Declaration names don't match for: " ++ name ++ " and " ++ name'
  env <- ask
  case lambda of
    (ConciseLambda pos idents lexpr) -> do
      assert (length argTypes - 1 == length idents) pos $ "number of arguments don't match in signature and definition of" ++ name
      env' <- addArgsTypesToEnv (map (\(Ident n) -> n) idents) argTypes pos
      let env'' = insert name (FunT pos argTypes) env'
      retType <- local (const env'') (evalType lexpr)
      assertType retType (last argTypes) pos
      return (insert name (FunT pos argTypes) env, Nothing)
    (LongLambda pos idents (Block _ stmts)) -> do
      assert (length argTypes - 1 == length idents) pos $ "number of arguments don't match in signature and definition of" ++ name
      env' <- addArgsTypesToEnv (map (\(Ident n) -> n) idents) argTypes pos
      let env'' = insert name (FunT pos argTypes) env'
      (_, retType) <- local (const env'') (typeCheckStmts stmts)
      assertType (fromMaybe (Void pos) retType) (last argTypes) pos
      return (insert name (FunT pos argTypes) env, Nothing)
    _ -> do
      t <- evalType lambda
      return (insert name t env, Nothing)


typeCheck (DataDecl pos (Udent name) params constructors) = do
  let dt = DataType pos (Udent name) params
  env <- addConstructorsToEnv constructors dt pos
  return (env, Nothing)

typeCheck (Cond pos e (Block _ stmts)) = do
  t <- evalType e
  assertType t (Bool Nothing) pos
  typeCheckStmts stmts

typeCheck (CondElse pos e (Block _ stmts) (Block _ stmts')) = do
  t <- evalType e
  assertType t (Bool Nothing) pos
  (_, ret) <- typeCheckStmts stmts
  (_, ret') <- typeCheckStmts stmts'
  case (ret, ret') of
    (Nothing, Nothing) -> continueTypeCheck
    (Just t1, Just t2) -> 
      (if compareType t1 t2 then
       (do currEnv <- ask
           return (currEnv, ret))
      else
        throwError $ ReturnTypeVary pos (show t1) (show t2))
    (_, _) -> throwError $ ReturnTypeVary pos (show ret) (show ret') 

typeCheck (Ret _ e) = do
  t <- evalType e
  env <- ask
  return (env, Just t)

typeCheck (VoidRet pos) = do
  env <- ask
  return (env, Just (Void pos))

typeCheck (SExp _ e) = do
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
