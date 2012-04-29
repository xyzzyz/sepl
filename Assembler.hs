{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Assembler where

import Control.Monad.Writer
import Control.Monad.State

import Data.List
import Data.Ord
import qualified Data.Map as Map

import ASM

data AssemblyEnv = AssemblyEnv {
  labels :: Map.Map String Int
}

type BFPrimitiveBlock = [BFPrimitive]

type Assembler = WriterT BFPrimitiveBlock (State AssemblyEnv)

prepareAssemblyEnv :: [BFSnippet] -> AssemblyEnv
prepareAssemblyEnv snippets = AssemblyEnv ls
  where isMain (BFSnippet "main" _ _) = True
        isMain _ = False
        snippets' = filter (not . isMain) snippets
        ls = foldr addLabel (Map.fromList [("$exit", 0), ("main", 1)]) (zip snippets' [2..])
        addLabel (BFSnippet name _ _, n) m = Map.insert name n m

getEnv :: Assembler AssemblyEnv
getEnv = lift get

putEnv :: AssemblyEnv -> Assembler ()
putEnv = lift . put

assemble :: BFSnippet -> Assembler ()
assemble = undefined

assembleSnippet :: BFSnippet -> Assembler BFPrimitiveBlock
assembleSnippet s = do
  r <- censor (const []) (listen (assemble s))
  let (_, block) = r
  return block

--assembleSnippets :: [BFSnippet] -> [BFPrimitiveBlock]
assembleSnippets ss = fst (evalState (runWriterT snippets') env)
  where snippets' = mapM assembleSnippet ss
        env = prepareAssemblyEnv ss


