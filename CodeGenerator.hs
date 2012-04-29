module CodeGenerator where

import Control.Monad.State
import Control.Monad.Writer

import Debug.Trace

import qualified Data.Map as Map

import AST
import ASM
import CPS

data GeneratorEnv = GeneratorEnv {
  functions :: Map.Map String CPSFun,
  currentFun :: String,
  assembledBlocks :: [BFSnippet]
  } deriving (Show)


emptyGeneratorEnv = GeneratorEnv Map.empty "" []

type GeneratorState = State GeneratorEnv

getFun :: String -> GeneratorState CPSFun
getFun name = do
  e <- get
  return $ functions e Map.! name

getCurrentFun :: GeneratorState CPSFun
getCurrentFun = do
  e <- get
  return $ (functions e Map.! currentFun e)

getVarID :: String -> GeneratorState Int
getVarID var = do
  f <- getCurrentFun
  let (CPSFun { variables = vars}) = f
  return $ vars Map.! var


generateJump :: CPSJump -> GeneratorState [BFASMInstruction]
generateJump (UseContinuation name) =
  return [SetTarget name]

generateJump (UseStackContinuation Nothing) =
  return [Push 0, DestroyFrame]

generateJump (UseStackContinuation (Just e)) = do
  e' <- generateExpr e
  return $ e' ++ [DestroyFrame]

generateJump (UseIfElse expr thn els) = do
  e' <- generateExpr expr
  return $ e' ++ [SetTargetIfThenElse thn els]

generateJump (UseCall name args arrs retAddr) = do
  f <- getFun name
  let strings = Map.toList (cpsStringLiterals f)
      vars = variables f
  args' <- mapM generateExpr args
  arrs' <- mapM generateExpr arrs
  return $ [NextCell] ++ concat args' ++ concat arrs'
    ++ replicate (length args + length arrs) PrevCell
    ++ [AllocateFrame strings (length arrs) (localsCount f) (length args) retAddr]

generateExprs :: [Expression] -> GeneratorState [BFASMInstruction]
generateExprs [] = return []
generateExprs (e:es) = do
  e' <- generateExpr e
  es' <- generateExprs es
  return $ e' ++ [Pop] ++ es'

-- everything is O(n^2), move to Data.Seq
generateExpr :: Expression -> GeneratorState [BFASMInstruction]
generateExpr (IntLiteral n) = return [Push n]
generateExpr Input = return [ASMInput]
generateExpr (Output expr) = do
  e' <- generateExpr expr
  return $ e' ++ [ASMOutput]
generateExpr (Variable name) = do
  i <- getVarID name
  return [GetVar i]
generateExpr (VarAssign name expr) = do
  e' <- generateExpr expr
  i <- getVarID name
  return $ e' ++ [SetVar i]
generateExpr (ArrAssign name ix val) = do
  val' <- generateExpr val
  ix' <- generateExpr ix
  i <- getVarID name
  return $ ix' ++ val' ++ [SetArr i]
generateExpr (ArrRef name ix) = do
  ix' <- generateExpr ix
  i <- getVarID name
  return $ ix' ++ [GetArrRef i]
generateExpr (Not expr) = do
  e <- generateExpr expr
  return $ e ++ [ASMNot]
generateExpr (Or e1 e2) = generateBinaryExpr ASMOr e1 e2
generateExpr (And e1 e2) = generateBinaryExpr ASMAnd e1 e2
generateExpr (Add e1 e2) = generateBinaryExpr ASMAdd e1 e2
generateExpr (Sub e1 e2) = generateBinaryExpr ASMSub e1 e2
generateExpr (Mul e1 e2) = generateBinaryExpr ASMMul e1 e2
generateExpr (Div e1 e2) = generateBinaryExpr ASMDiv e1 e2
generateExpr (Mod e1 e2) = generateBinaryExpr ASMMod e1 e2
generateExpr (Equals e1 e2) = generateBinaryExpr ASMEquals e1 e2
generateExpr (LessThan e1 e2) = generateBinaryExpr ASMLessThan e1 e2
generateExpr (GreaterThan e1 e2) = generateBinaryExpr ASMGreaterThan e1 e2
generateExpr (LessOrEqual e1 e2) = generateBinaryExpr ASMLessOrEqual e1 e2
generateExpr (GreaterOrEqual e1 e2) = generateBinaryExpr ASMGreaterOrEqual e1 e2

generateBinaryExpr op e1 e2 = do
  e1' <- generateExpr e1
  e2' <- generateExpr e2
  return $ e1' ++ e2' ++ [op]

generateBlockCode :: CPSBlock -> GeneratorState BFSnippet

generateBlockCode (CPSPopBlock n Nothing jump) = do
  jump' <- generateJump jump
  return $ BFSnippet n [Pop] jump'

generateBlockCode (CPSPopBlock n (Just var) jump) = do
  i <- getVarID var
  jump' <- generateJump jump
  return $ BFSnippet n [SetVar i] jump'

generateBlockCode (CPSBlock n exprs jump) = do
  es <- generateExprs exprs
  jump' <- generateJump jump
  return $ BFSnippet n es jump'


generateFunCode :: CPSFun -> GeneratorState ()
generateFunCode (CPSFun { cpsFunName = n,
                          cpsStringLiterals = strings,
                          variables = vars,
                          bodyBlocks = cpsBlocks }) = do
  env <- get
  put $ env { currentFun = n }

  blocks <- mapM generateBlockCode cpsBlocks
  env' <- get
  put $ env' { assembledBlocks = blocks ++ assembledBlocks env' }
  return ()

generateFunsCode :: [CPSFun] -> [BFSnippet]
generateFunsCode funs = assembledBlocks state
  where state = execState (mapM_ generateFunCode funs) env
        env = emptyGeneratorEnv {functions = funs'}
        funs' = Map.fromList (map makeFunPair funs)
        makeFunPair :: CPSFun -> (String, CPSFun)
        makeFunPair f = (cpsFunName f, f)

