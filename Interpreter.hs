{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import Control.Monad.State
import Control.Monad.Identity

import Data.Char
import Data.Array
import qualified Data.Map as Map

import AST

data BFVar = BFInt Int | BFBool Bool | BFIntArray (Array Int Int) | BFVoid
           deriving (Show)

type VarEnv = Map.Map String BFVar
type FunEnv = Map.Map String FunctionDefinition

data Env = Env {
  varEnv :: VarEnv,
  funEnv :: FunEnv
  } deriving (Show)

emptyEnv = Env {varEnv = Map.empty, funEnv = Map.empty }

type InterpreterState = StateT Env IO

class BFValue a where
  extractBF :: BFVar -> a
  packBF :: a -> BFVar
  defaultValue :: a

instance BFValue Int where
  extractBF (BFInt i) = i
  extractBF _ = error "RUNTIME ERROR: type mismatch"
  packBF i = BFInt i
  defaultValue = 0

instance BFValue Bool where
  extractBF (BFBool b) = b
  extractBF _ = error "RUNTIME ERROR: type mismatch"
  packBF b = BFBool b
  defaultValue = False

instance BFValue (Array Int Int) where
  extractBF (BFIntArray a) = a
  extractBF _ = error "RUNTIME ERROR: type mismatch"
  packBF a = BFIntArray a
  defaultValue = listArray (0, 0) []

getVar :: BFValue a => String -> InterpreterState a
getVar name = do
  e <- get
  return $ extractBF ((varEnv e) Map.! name)

setVar :: BFValue a => String -> a -> InterpreterState ()
setVar name v = do
  e <- get
  put $ e { varEnv = (Map.insert name (packBF v) (varEnv e)) }

getType :: String -> InterpreterState Type
getType name = do
  e <- get
  case varEnv e Map.! name of
    BFInt _ -> return Int
    BFBool _ -> return Bool
    BFIntArray _ -> return IntArray

getFun :: String -> InterpreterState FunctionDefinition
getFun name = do
  e <- get
  return $ (funEnv e) Map.! name

packBFM :: BFValue a => InterpreterState a -> InterpreterState BFVar
packBFM = liftM packBF

extractBFM :: BFValue a => InterpreterState BFVar -> InterpreterState a
extractBFM = liftM extractBF

returnBF :: BFValue a => a -> InterpreterState BFVar
returnBF = return . packBF

evalExprValue :: BFValue a => Expression -> InterpreterState a
evalExprValue = extractBFM . evalExpr

evalExpr :: Expression -> InterpreterState BFVar
evalExpr (IntLiteral n) = returnBF n
evalExpr (StringLiteral s) = returnBF $ listArray (0, length s) (map ord s ++ [0])
evalExpr Input = lift $ do
  c <- getChar
  return $ BFInt (ord c)

evalExpr (Sizeof name) = do
  arr <- getVar name
  let (0, n) = bounds (arr :: Array Int Int)
  returnBF (n+1)
  
evalExpr (Variable var) = do
  t <- getType var
  case t of
    Int -> packBFM (getVar var :: InterpreterState Int)
    Bool -> packBFM (getVar var :: InterpreterState Bool)
    IntArray -> packBFM (getVar var :: InterpreterState (Array Int Int))

evalExpr (VarAssign var expr) = do
  t <- getType var
  case t of
    Int -> packBFM (setVar' var expr :: InterpreterState Int)
    Bool -> packBFM (setVar' var expr :: InterpreterState Bool)
    IntArray -> packBFM (setVar' var expr :: InterpreterState (Array Int Int))
    where setVar' :: BFValue a => String -> Expression -> InterpreterState a
          setVar' name expr = do
            val <- evalExprValue expr
            setVar name val
            return val

evalExpr (ArrAssign var ix expr) = do
  ix' <- evalExprValue ix
  val <- evalExprValue expr
  arr <- getVar var
  setVar var ((arr // [(ix', val)]) :: Array Int Int)
  returnBF val

evalExpr (ArrRef var ix) = do
  ix' <- evalExprValue ix
  arr <- getVar var
  returnBF $ (arr :: Array Int Int) ! ix'

evalExpr (Not expr) = do
  val <- evalExprValue expr
  returnBF (not val)

evalExpr (Or e1 e2) = evalBinaryExpr (||) e1 e2
evalExpr (And e1 e2) = evalBinaryExpr (&&) e1 e2
evalExpr (Nand e1 e2) = evalBinaryExpr nand e1 e2
  where nand p q = not (p && q)

evalExpr (Add e1 e2) = evalBinaryExpr ((+) :: Int -> Int -> Int) e1 e2
evalExpr (Sub e1 e2) = evalBinaryExpr ((-) :: Int -> Int -> Int) e1 e2
evalExpr (Mul e1 e2) = evalBinaryExpr ((*) :: Int -> Int -> Int) e1 e2
evalExpr (Div e1 e2) = evalBinaryExpr (div :: Int -> Int -> Int) e1 e2
evalExpr (Equals e1 e2) = evalBinaryExpr ((==) :: Int -> Int -> Bool) e1 e2
evalExpr (LessThan e1 e2) = evalBinaryExpr ((<) :: Int -> Int -> Bool) e1 e2
evalExpr (GreaterThan e1 e2) = evalBinaryExpr ((>) :: Int -> Int -> Bool) e1 e2
evalExpr (LessOrEqual e1 e2) = evalBinaryExpr ((<=) :: Int -> Int -> Bool) e1 e2
evalExpr (GreaterOrEqual e1 e2) = evalBinaryExpr ((>=) :: Int -> Int -> Bool) e1 e2

evalExpr (Call name givenArgs givenArrs) = do
  fun <- getFun name
  args' <- mapM evalExpr givenArgs
  arrs' <- mapM evalExpr givenArrs
  saveEnv $ do
    updateEnv fun args' arrs'
    v <- evalStmt (body fun)
    case v of
      Left ret -> return ret
      Right () -> error "RUNTIME ERROR: expected return"
  where updateEnv fun args' arrs' = do
          zipWithM_ initVarVal (args fun) args'
          zipWithM_ initArr (arrays fun) arrs'
          mapM_ initVarDefault (locals fun)
        initVarDefault (Int, name) = setVar name (defaultValue :: Int)
        initVarDefault (Bool, name) = setVar name (defaultValue :: Bool)
        initVarDefault (IntArray, name) = setVar name (defaultValue :: Array Int Int)
        initVarVal (Int, name) (BFInt val) = setVar name val
        initVarVal (Bool, name) (BFBool val) = setVar name val
        initVarVal (IntArray, name) (BFIntArray val) = setVar name val
        initVarVal p q = error $ "RUNTIME ERROR: wrong argument type: " ++ show p ++ " " ++ show q
        initArr (IntArray, name) (BFInt n) = setVar name (listArray (0, n-1) (replicate n 0) :: Array Int Int)

evalBinaryExpr f e1 e2 = do
  v1 <- evalExprValue e1
  v2 <- evalExprValue e2
  returnBF (f v1 v2)

saveEnv act = do
  e <- get
  val <- act
  put e
  return val

evalStmt :: Statement -> InterpreterState (Either BFVar ())
evalStmt (ExpressionStatement expr) = do
  evalExpr expr
  return . return $ ()

evalStmt (BlockStatement stmts) = evalStmts stmts
  where evalStmts [] = return . return $ ()
        evalStmts (s:ss) = do
          s <- evalStmt s
          case s of
            Right () -> evalStmts ss
            l -> return l

evalStmt s@(WhileStatement cond stmt) = do
  val <- evalExpr cond
  case val of
    BFBool False -> return . return $ ()
    BFBool True  -> do
      res <- evalStmt stmt
      case res of
        Right () -> evalStmt s
        l -> return l
    _ -> error "RUNTIME ERROR: expected bool"

evalStmt (IfElseStatement cond thn els) = do
  val <- evalExpr cond
  case val of
    BFBool True -> evalStmt thn
    BFBool False -> evalStmt els

evalStmt (OutputStatement expr) = do
  val <- evalExpr expr
  case val of
    BFInt n -> lift (putChar (chr n))
    _ -> error "RUNTIME ERROR: expected integer"
  return . return $ ()

evalStmt (ReturnStatement Nothing) = do
  return $ Left BFVoid
evalStmt (ReturnStatement (Just expr)) = do
  val <- evalExpr expr
  return $ Left val

evalTranslationUnit :: TranslationUnit -> IO Env
evalTranslationUnit defs = execStateT callMain emptyEnv
  where callMain :: InterpreterState ()
        callMain = do
          mapM_ addDef defs
          evalExpr (Call "main" [] [])
          return ()
            where addDef :: FunctionDefinition -> InterpreterState ()
                  addDef def = do
                    e <- get
                    let newEnv = Map.insert (name def) def (funEnv e)
                    put $ e { funEnv = newEnv }