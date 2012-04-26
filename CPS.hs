module CPS where

import Control.Monad.State
import Control.Monad.Identity

import AST

data CPSJump = UseStackContinuation (Maybe Expression)
             | UseContinuation String
             | UseIfElse Expression String String
             | UseCall String [Expression] [Expression] String
             deriving (Show)


data CPSBlock = CPSBlock String [Expression] CPSJump
              | CPSPopBlock String (Maybe String) CPSJump
              deriving (Show)

--data CPSFun

data CPSEnv = CPSEnv {
  nextID :: Int,
  funName :: String
  } deriving (Show)

makeCPSEnv name = CPSEnv 0 name

type CPSTransformerState = State CPSEnv

getNextID :: CPSTransformerState String
getNextID = do
  e <- get
  let id = nextID e
      n  = funName e
  put $ e { nextID = id + 1 }
  return $ "$c" ++ n ++ show id


makeCPSBlock name exprs jump = CPSBlock name (reverse exprs) jump

cpsTransformStmts' [] cur curName jump = return [makeCPSBlock curName cur jump]

cpsTransformStmts' (BlockStatement bs : ss) cur curName jump = do
  n1 <- getNextID
  n2 <- getNextID
  block <- cpsTransformStmts' bs [] n1 (UseContinuation n2)
  rest <- cpsTransformStmts' ss [] n2 jump
  return $ makeCPSBlock curName cur (UseContinuation n1)
    : (block ++ rest)

cpsTransformStmts' (ExpressionStatement e : ss) cur curName jump = do
  cpsTransformStmts' ss (e : cur) curName jump

cpsTransformStmts' (WhileStatement test (BlockStatement body) : ss) cur curName jump = do
  wn <- getNextID
  bn <- getNextID
  rn <- getNextID
  body' <- cpsTransformStmts' body [] bn (UseContinuation wn)
  rest <- cpsTransformStmts' ss [] rn jump
  let whileBlock = makeCPSBlock wn [] (UseIfElse test bn rn)
  return $ makeCPSBlock curName cur (UseContinuation wn) : whileBlock : (body' ++ rest)

cpsTransformStmts' (IfElseStatement test (BlockStatement thn) (BlockStatement els)
                    : ss) cur curName jump = do
  thnn <- getNextID
  elsn <- getNextID
  rn <- getNextID
  thn' <- cpsTransformStmts' thn [] thnn (UseContinuation rn)
  els' <- cpsTransformStmts' els [] elsn (UseContinuation rn)
  rest <- cpsTransformStmts' ss [] rn jump
  return $ makeCPSBlock curName cur (UseIfElse test thnn elsn)
    : (thn' ++ els' ++ rest)

cpsTransformStmts' (ReturnStatement ret : ss) cur curName jump = do
  return $ [makeCPSBlock curName cur (UseStackContinuation ret)]

cpsTransformStmts' (Call assign name args arrs : ss) cur curName jump = do
  nn <- getNextID
  rn <- getNextID
  rest <- cpsTransformStmts' ss [] rn jump
  return $ makeCPSBlock curName cur (UseCall name args arrs nn)
    : CPSPopBlock nn assign (UseContinuation rn)
    : rest

cpsTransformDef :: FunctionDefinition -> [CPSBlock]
cpsTransformDef (FunctionDefinition { name = name,
                                      args = args,
                                      locals = locals,
                                      arrays = arrays,
                                      body = (BlockStatement body) }) =
  evalState transform (makeCPSEnv name)
    where transform = cpsTransformStmts' body [] name (UseStackContinuation Nothing)