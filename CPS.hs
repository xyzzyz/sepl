module CPS where

import Control.Monad.State
import Control.Monad.Identity

import qualified Data.Map as Map

import AST

data CPSJump = UseStackContinuation (Maybe Expression)
             | UseContinuation String
             | UseIfElse Expression String String
             | UseCall String [Expression] [Expression] String
             deriving (Show)


data CPSBlock = CPSBlock String [Expression] CPSJump
              | CPSPopBlock String (Maybe String) CPSJump
              deriving (Show)

data CPSFun = CPSFun {
  cpsFunName :: String,
  cpsStringLiterals :: Map.Map String String,
  variables :: Map.Map String Int,
  localsCount :: Int,
  bodyBlocks :: [CPSBlock]
  } deriving (Show)

data CPSEnv = CPSEnv {
  nextID :: Int,
  funName :: String,
  stringLiterals :: Map.Map String String
  } deriving (Show)

makeCPSEnv :: String -> CPSEnv
makeCPSEnv name = CPSEnv 0 name Map.empty

type CPSTransformerState = State CPSEnv

getFunName :: CPSTransformerState String
getFunName = do
  e <- get
  return $ funName e

getNextID :: CPSTransformerState Int
getNextID = do
  e <- get
  let id = nextID e
  put $ e { nextID = id + 1 }
  return id

getNextContID :: String -> CPSTransformerState String
getNextContID str = do
  id <- getNextID
  n <- getFunName
  return $ "$" ++ n ++ "$" ++ str ++ show id

getNextStringID :: CPSTransformerState String
getNextStringID = do
  id <- getNextID
  n <- getFunName
  return $ "$" ++ n ++ "$s"++ show id


transformExpr :: Expression -> CPSTransformerState Expression
transformExpr (StringLiteral s) = do
  n <- getNextStringID
  e <- get
  put $ e { stringLiterals = Map.insert n s (stringLiterals e) }
  return $ Variable n

transformExpr (VarAssign var expr) = do
  e <- transformExpr expr
  return $ VarAssign var e
transformExpr (Nand e1 e2) = return $ Not (And e1 e2)
transformExpr (BoolLiteral True) = return $ IntLiteral 1
transformExpr (BoolLiteral False) = return $ IntLiteral 0
transformExpr e = return e

makeCPSBlock name exprs jump = CPSBlock name (reverse exprs) jump

cpsTransformStmts' [] cur curName jump = return [makeCPSBlock curName cur jump]

cpsTransformStmts' (BlockStatement bs : ss) cur curName jump = do
  n1 <- getNextContID "block"
  n2 <- getNextContID "after_block"
  block <- cpsTransformStmts' bs [] n1 (UseContinuation n2)
  rest <- cpsTransformStmts' ss [] n2 jump
  return $ makeCPSBlock curName cur (UseContinuation n1)
    : (block ++ rest)

cpsTransformStmts' (ExpressionStatement e : ss) cur curName jump = do
  e' <- transformExpr e
  cpsTransformStmts' ss (e' : cur) curName jump

cpsTransformStmts' (WhileStatement test (BlockStatement body) : ss) cur curName jump = do
  test' <- transformExpr test
  wn <- getNextContID "while_test"
  bn <- getNextContID "while_body"
  rn <- getNextContID "after_while"
  body' <- cpsTransformStmts' body [] bn (UseContinuation wn)
  rest <- cpsTransformStmts' ss [] rn jump
  let whileBlock = makeCPSBlock wn [] (UseIfElse test' bn rn)
  return $ makeCPSBlock curName cur (UseContinuation wn) : whileBlock : (body' ++ rest)

cpsTransformStmts' (IfElseStatement test (BlockStatement thn) (BlockStatement els)
                    : ss) cur curName jump = do
  test' <- transformExpr test
  thnn <- getNextContID "then"
  elsn <- getNextContID "else"
  rn <- getNextContID "after_if"
  thn' <- cpsTransformStmts' thn [] thnn (UseContinuation rn)
  els' <- cpsTransformStmts' els [] elsn (UseContinuation rn)
  rest <- cpsTransformStmts' ss [] rn jump
  return $ makeCPSBlock curName cur (UseIfElse test' thnn elsn)
    : (thn' ++ els' ++ rest)

cpsTransformStmts' (ReturnStatement ret : ss) cur curName jump = do
  return $ [makeCPSBlock curName cur (UseStackContinuation ret)]

cpsTransformStmts' (Call assign name args arrs : ss) cur curName jump = do
  nn <- getNextContID "pop"
  rn <- getNextContID "after_call"
  rest <- cpsTransformStmts' ss [] rn jump
  args' <- mapM transformExpr args
  arrs' <- mapM transformExpr arrs
  return $ makeCPSBlock curName cur (UseCall name args' arrs' nn)
    : CPSPopBlock nn assign (UseContinuation rn)
    : rest

cpsTransformDef :: FunctionDefinition -> CPSFun
cpsTransformDef (FunctionDefinition { name = name,
                                      args = args,
                                      locals = locals,
                                      arrays = arrays,
                                      body = (BlockStatement body) }) =
  CPSFun name strings vars (length locals) blocks
    where (blocks, endState) = runState transform (makeCPSEnv name)
          transform = cpsTransformStmts' body [] name (UseStackContinuation Nothing)
          strings = stringLiterals endState
          vars = Map.fromList (zip (Map.keys strings ++ map snd (arrays ++ args ++ locals)) [1..])

cpsTransformTranslationUnit :: TranslationUnit -> [CPSFun]
cpsTransformTranslationUnit = map cpsTransformDef
