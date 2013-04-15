{-# LANGUAGE TemplateHaskell #-}
module  CodeGenerator where

import Control.Monad.State hiding (mapM)
import Control.Monad.Writer hiding (mapM)
import Control.Lens hiding ((<|), (|>))
import Control.Applicative((<$>), (<*>))

import Debug.Trace

import qualified Data.Map as Map
import qualified Data.Sequence as S
import Data.Sequence (Seq, empty, (<|), (|>), (><))
import Data.Foldable(fold)
import Data.Traversable

import Prelude hiding (mapM)

import AST
import ASM
import CPS

data GeneratorEnv = GeneratorEnv {
  _functions :: Map.Map String CPSFun,
  _currentFun :: String,
  _assembledBlocks :: Seq BFSnippet
  } deriving (Show)

$( makeLenses ''GeneratorEnv )

emptyGeneratorEnv = GeneratorEnv Map.empty "" empty

type GeneratorState = State GeneratorEnv

getFun :: String -> GeneratorState CPSFun
getFun name = do
  fs <- use functions
  return $ fs Map.! name

getCurrentFun :: GeneratorState CPSFun
getCurrentFun = do
  fs <- use functions
  current <- use currentFun
  return $ fs  Map.! current

getVarID :: String -> GeneratorState Int
getVarID var = do
  f <- getCurrentFun
  return $ (f ^. variables) Map.! var


generateJump :: CPSJump -> GeneratorState (Seq BFASMInstruction)
generateJump (UseContinuation name) =
  return $ SetTarget name <| empty

generateJump (UseStackContinuation Nothing) =
  return $ Push 0 <| DestroyFrame <| empty

generateJump (UseStackContinuation (Just e)) = do
  e' <- generateExpr e
  return $ e' |> DestroyFrame

generateJump (UseIfElse expr thn els) = do
  e' <- generateExpr expr
  return $ e' |> SetTargetIfThenElse thn els

generateJump (UseCall name args arrs retAddr) = do
  f <- getFun name
  let strings = Map.toList (f ^. cpsStringLiterals)
      vars = f ^. variables
  args' <- mapM generateExpr args
  arrs' <- mapM generateExpr arrs
  let n = length args' + length arrs' 
  return $ (NextPos <| mconcat arrs' >< mconcat args')
    >< (S.replicate (n+1) PrevPos
        |> PutMarker)
    >< (S.replicate n NextPos
        |> AllocateFrame name strings (length arrs) (f ^. localsCount) (length args) retAddr)

generateExprs :: Seq Expression -> GeneratorState (Seq BFASMInstruction)
generateExprs es = fold <$> f `traverse` es
  where f :: Expression -> GeneratorState (Seq BFASMInstruction)
        f = fmap (|> Pop) . generateExpr

generateExpr :: Expression -> GeneratorState (Seq BFASMInstruction)
generateExpr (IntLiteral n) = return $ Push n <| empty
generateExpr Input = return $ ASMInput <| empty
generateExpr (Output expr) = (|>) <$> generateExpr expr <*> return ASMOutput
generateExpr (Variable name) = (empty |>) <$> GetVar <$> getVarID name
generateExpr (VarAssign name expr) = (|>) <$> generateExpr expr
                                          <*> (SetVar <$> getVarID name)
generateExpr (ArrAssign name ix val) = do
  val' <- generateExpr val
  ix' <- generateExpr ix
  i <- getVarID name
  return $ ix' >< (val' |> SetArr i)
generateExpr (ArrRef name ix) = do
  ix' <- generateExpr ix
  i <- getVarID name
  return $ ix' |> GetArrRef i
generateExpr (Not expr) = do
  e <- generateExpr expr
  return $ e |> ASMNot
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
  return $ e1'>< (e2' |> op)

generateBlockCode :: CPSBlock -> GeneratorState BFSnippet
generateBlockCode (CPSPopBlock n Nothing jump) = do
  jump' <- generateJump jump
  return $ BFSnippet n (PopArg <| Pop <| empty) jump'

generateBlockCode (CPSPopBlock n (Just var) jump) = do
  i <- getVarID var
  jump' <- generateJump jump
  return $ BFSnippet n (PopArg <| SetVar i <| Pop <| empty) jump'

generateBlockCode (CPSExprBlock n exprs jump) = do
  es <- generateExprs exprs
  jump' <- generateJump jump
  return $ BFSnippet n es jump'

generateFunCode :: CPSFun -> GeneratorState ()
generateFunCode cpsFun = do
  currentFun .= (cpsFun^.cpsFunName)
  blocks <- mapM generateBlockCode (cpsFun^.bodyBlocks)

  assembledBlocks %= (blocks ><)
  return ()

generateFunsCode :: [CPSFun] -> Seq BFSnippet
generateFunsCode funs = state^.assembledBlocks
  where state = execState (mapM_ generateFunCode funs) env
        env = emptyGeneratorEnv { _functions = funs'}
        funs' = Map.fromList (map makeFunPair funs)
        makeFunPair :: CPSFun -> (String, CPSFun)
        makeFunPair f = (f^.cpsFunName , f)

