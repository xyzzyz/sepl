{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module TypeChecker(typeCheckTranslationUnit) where

import AST
import Control.Monad.State
import Control.Monad.Error

import Data.List
import Data.Maybe
import Data.Tuple
import qualified Data.Map as Map

import AST

type TypeEnv = Map.Map String Type

type FunEnv = Map.Map String (Type, [Type], Int)

data Env = Env {
  varEnv :: TypeEnv,
  funEnv  :: FunEnv,
  curRetType :: Maybe Type
  } deriving (Show)

emptyEnv = Env Map.empty Map.empty Nothing

data TypeError = TypeMismatch Type Type
               | MultipleVariableDefinitionError Type Type
               | WrongArgumentCount String Int Int
               | UnknownSymbolError String
               | OtherTypeError String
                 deriving (Show)

instance Error TypeError where
  noMsg  = OtherTypeError "type error occured"
  strMsg = OtherTypeError

newtype TypeChecker a = Checker {
  runChecker :: ErrorT TypeError (State Env) a
  } deriving (Monad, MonadError TypeError)

liftChecker m = Checker (lift m)

getEnv   = liftChecker get
putEnv e = liftChecker $ put e

getType name = do
  e <- getEnv
  case Map.lookup name (varEnv e) of
    Nothing -> throwError (UnknownSymbolError name)
    Just t  -> return t

getFunType name = do
  e <- getEnv
  case Map.lookup name (funEnv e) of
    Nothing -> throwError (UnknownSymbolError name)
    Just t  -> return t


ensureType expected got = do
  when (got /= expected) $ 
    throwError (TypeMismatch expected got)

typeCheckBinaryOp expect ret e1 e2 = do
  typ1 <- typeCheckExpr e1
  ensureType expect typ1

  typ2 <- typeCheckExpr e2
  ensureType expect typ2

  return ret

typeCheckBinaryNumOp   = typeCheckBinaryOp Int Int
typeCheckBinaryRelOp   = typeCheckBinaryOp Int Bool
typeCheckBinaryLogicOp = typeCheckBinaryOp Bool Bool


typeCheckExpr (IntLiteral _) = return Int

typeCheckExpr (StringLiteral _ ) = return IntArray

typeCheckExpr Input = return Int

typeCheckExpr (Sizeof name) = do
  t <- getType name
  ensureType IntArray t
  return Int

typeCheckExpr (Variable name) = getType name

typeCheckExpr (VarAssign name expr) = do
  expected <- getType name
  typ <- typeCheckExpr expr
  ensureType expected typ
  return typ

typeCheckExpr (ArrAssign name ix expr) = do
  symbolType <- getType name
  ensureType IntArray symbolType

  ixType <- typeCheckExpr ix
  ensureType Int ixType

  valType <- typeCheckExpr expr
  ensureType Int valType

  return Int

typeCheckExpr (ArrRef name expr) = do
  t <- typeCheckExpr expr
  ensureType Int t
  at <- getType name
  ensureType IntArray at
  return Int

typeCheckExpr (Add e1 e2) = typeCheckBinaryNumOp e1 e2
typeCheckExpr (Sub e1 e2) = typeCheckBinaryNumOp e1 e2
typeCheckExpr (Mul e1 e2) = typeCheckBinaryNumOp e1 e2
typeCheckExpr (Div e1 e2) = typeCheckBinaryNumOp e1 e2
typeCheckExpr (Mod e1 e2) = typeCheckBinaryNumOp e1 e2

typeCheckExpr (Equals e1 e2) = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  ensureType Int t1
  ensureType Int t2
  return Bool

typeCheckExpr (LessThan e1 e2)       = typeCheckBinaryRelOp e1 e2
typeCheckExpr (GreaterThan e1 e2)    = typeCheckBinaryRelOp e1 e2
typeCheckExpr (LessOrEqual e1 e2)    = typeCheckBinaryRelOp e1 e2
typeCheckExpr (GreaterOrEqual e1 e2) = typeCheckBinaryRelOp e1 e2

typeCheckExpr (And e1 e2)  = typeCheckBinaryLogicOp e1 e2
typeCheckExpr (Or e1 e2)   = typeCheckBinaryLogicOp e1 e2
typeCheckExpr (Nand e1 e2) = typeCheckBinaryLogicOp e1 e2

typeCheckExpr (Not e) = do
  t <- typeCheckExpr e
  ensureType Bool t
  return t

typeCheckExpr (Call name args arrays) = do
  (ret, expectedTypes, expectedArrays) <- getFunType name
  gotTypes <- mapM typeCheckExpr args
  gotArrays <- mapM typeCheckExpr arrays
  mapM_ (ensureType Int) gotArrays

  when (length expectedTypes /= length gotTypes) $
    throwError $ WrongArgumentCount name (length expectedTypes) (length gotTypes)

  when (expectedArrays /= length gotArrays) $
    throwError $ WrongArgumentCount name expectedArrays (length gotArrays)

  mapM_ (uncurry ensureType) (zip expectedTypes gotTypes)
  return ret

typeCheckStmt (ExpressionStatement e) = do
  typeCheckExpr e
  return ()

typeCheckStmt (BlockStatement bs) = mapM_ typeCheckStmt bs

typeCheckStmt (WhileStatement cond body) = do
  c <- typeCheckExpr cond
  ensureType Bool c
  typeCheckStmt body

typeCheckStmt (IfElseStatement cond thn els) = do
  c <- typeCheckExpr cond
  ensureType Bool c
  typeCheckStmt thn
  typeCheckStmt els

typeCheckStmt (OutputStatement expr) = do
  c <- typeCheckExpr expr
  ensureType Int c
  return ()

typeCheckStmt (ReturnStatement maybeExpr) = do
  t <- case maybeExpr of
    Nothing -> return Void
    Just e  -> typeCheckExpr e
  e <- getEnv
  ensureType (fromMaybe Void (curRetType e)) t
  return ()

typeCheck (FunctionDefinition {retType = retType,
                               name = name,
                               args = args,
                               arrays = arrays,
                               locals = locals,
                               body = body}) = do
  checkMultipleDefs $ args ++ locals ++ arrays
  env <- getEnv
  let updatedFunEnv = env { funEnv = Map.insert name (retType, map fst args, length arrays) (funEnv env)}
  putEnv $ updatedFunEnv { varEnv = Map.fromList (map swap (args ++ locals ++ arrays)),
                           curRetType = Just retType}
  typeCheckStmt body
  putEnv updatedFunEnv

  where checkMultipleDefs [] = return ()
        checkMultipleDefs ((typ, name) : rest) = case find ((== name) . snd) rest of
          Nothing -> checkMultipleDefs rest
          Just (typ', _) -> throwError $ MultipleVariableDefinitionError typ typ'

typeChecker p = mapM_ typeCheck p

runTypeChecker p s = case runState (runErrorT (runChecker p)) s of
  (Left err, _) -> Left err
  (Right r, bs) -> Right (r, bs)

typeCheckTranslationUnit p = runTypeChecker (typeChecker p) emptyEnv
