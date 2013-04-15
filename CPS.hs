{-# LANGUAGE TemplateHaskell #-}

module CPS where

import Data.Sequence (Seq, empty, singleton)
import Data.Traversable (traverse)
import Control.Lens

import Control.Applicative hiding (empty)

import Control.Monad.RWS

import qualified Data.Map as Map
import Prelude hiding (mapM)
import AST

data CPSJump = UseStackContinuation (Maybe Expression)
             | UseContinuation String
             | UseIfElse Expression String String
             | UseCall String [Expression] [Expression] String
             deriving (Show)


data CPSBlock = CPSExprBlock String (Seq Expression) CPSJump
              | CPSPopBlock String (Maybe String) CPSJump
              deriving (Show)

data CPSFun = CPSFun {
  _cpsFunName :: String,
  _cpsStringLiterals :: Map.Map String String,
  _variables :: Map.Map String Int,
  _localsCount :: Int,
  _bodyBlocks :: Seq CPSBlock
  } deriving (Show)

$( makeLenses ''CPSFun )


data CPSState = CPSState {
  _nextID :: Int,
  _stringLiterals :: Map.Map String String
  } deriving (Show)

$( makeLenses ''CPSState )

emptyCPSState :: CPSState
emptyCPSState = CPSState 0 Map.empty

type CPSTransformer = RWS String (Seq CPSBlock) CPSState

getFunName :: CPSTransformer String
getFunName = ask

getNextID :: CPSTransformer Int
getNextID = nextID <+= 1

getNextContID :: String -> CPSTransformer String
getNextContID str = do
  id <- getNextID
  n <- getFunName
  return $ "$" ++ n ++ "$" ++ str ++ show id

getNextStringID :: CPSTransformer String
getNextStringID = do
  id <- getNextID
  n <- getFunName
  return $ "$" ++ n ++ "$s"++ show id


transformExpr :: Expression -> CPSTransformer Expression
transformExpr (StringLiteral s) = do
  n <- getNextStringID
  stringLiterals . at n .= Just s
  return $ Variable n

transformExpr (VarAssign var expr) = do
  e <- transformExpr expr
  return $ VarAssign var e
transformExpr (Nand e1 e2) = return $ Not (And e1 e2)
transformExpr (BoolLiteral True) = return $ IntLiteral 1
transformExpr (BoolLiteral False) = return $ IntLiteral 0
transformExpr e = return e

pushBlock :: CPSBlock -> CPSTransformer ()
pushBlock = tell . singleton

cpsTransformStmts' :: [Statement] -> Seq Expression -> String -> CPSJump -> CPSTransformer ()
cpsTransformStmts' [] cur curName jump = do
  pushBlock $ CPSExprBlock curName cur jump

cpsTransformStmts' (BlockStatement bs : ss) cur curName jump = do
  n1 <- getNextContID "block"
  n2 <- getNextContID "after_block"
  cpsTransformStmts' bs empty n1 (UseContinuation n2)
  cpsTransformStmts' ss empty n2 jump
  pushBlock $ CPSExprBlock curName cur (UseContinuation n1)

cpsTransformStmts' (ExpressionStatement e : ss) cur curName jump = do
  e' <- transformExpr e
  cpsTransformStmts' ss (cur |> e) curName jump

cpsTransformStmts' (WhileStatement test (BlockStatement body) : ss) cur curName jump = do
  test' <- transformExpr test
  wn <- getNextContID "while_test"
  bn <- getNextContID "while_body"
  rn <- getNextContID "after_while"
  cpsTransformStmts' body empty bn (UseContinuation wn)
  cpsTransformStmts' ss empty rn jump
  pushBlock $ CPSExprBlock wn empty (UseIfElse test' bn rn)
  pushBlock $ CPSExprBlock curName cur (UseContinuation wn)

cpsTransformStmts' (IfElseStatement test (BlockStatement thn) (BlockStatement els)
                    : ss) cur curName jump = do
  test' <- transformExpr test
  thnn <- getNextContID "then"
  elsn <- getNextContID "else"
  rn <- getNextContID "after_if"
  cpsTransformStmts' thn empty thnn (UseContinuation rn)
  cpsTransformStmts' els empty elsn (UseContinuation rn)
  cpsTransformStmts' ss empty rn jump
  pushBlock $ CPSExprBlock curName cur (UseIfElse test' thnn elsn)


cpsTransformStmts' (ReturnStatement ret : ss) cur curName jump = do
  ret' <- transformExpr `traverse` ret
  pushBlock $ CPSExprBlock curName cur (UseStackContinuation ret')

cpsTransformStmts' (Call assign name args arrs : ss) cur curName jump = do
  nn <- getNextContID "pop"
  rn <- getNextContID "after_call"
  cpsTransformStmts' ss empty rn jump
  args' <- mapM transformExpr args
  arrs' <- mapM transformExpr arrs
  pushBlock $ CPSExprBlock curName cur (UseCall name args' arrs' nn)
  pushBlock $ CPSPopBlock nn assign (UseContinuation rn)

cpsTransformDef :: FunctionDefinition -> CPSFun
cpsTransformDef (FunctionDefinition { name = name,
                                      args = args,
                                      locals = locals,
                                      arrays = arrays,
                                      body = (BlockStatement body) }) =
  CPSFun name strings vars (Prelude.length locals) blocks
    where (endState, blocks) = execRWS transform name emptyCPSState
          transform = cpsTransformStmts' body empty name (UseStackContinuation Nothing)
          strings =  endState ^. stringLiterals
          vars = Map.fromList (Prelude.zip (Map.keys strings ++ map snd (arrays ++ args ++ locals)) [1..])

cpsTransformTranslationUnit :: TranslationUnit -> [CPSFun]
cpsTransformTranslationUnit unit = map cpsTransformDef unit
