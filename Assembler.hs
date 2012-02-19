{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Assembler(BFPrimitive(..), BFSnippet(..), BFAsmInstruction(..), AssemblyEnv(..), 
                 runCodeGenerator,
                 prim, next, prev, dec, inc, loop,
                 fromBF, zero, clearNext, copyTimes, copyTo,
                 emit,
                 allocateBlocks) where

import Control.Monad.Writer
import Control.Monad.State

import Data.List
import Data.Ord
import qualified Data.Map as Map


data BFPrimitive = Next | Prev | Inc | Dec | Read | Print | Loop | EndLoop
                 deriving (Eq)

instance Show BFPrimitive where
  show Next    = ">"
  show Prev    = "<"
  show Inc     = "+"
  show Dec     = "-"
  show Read    = ","
  show Print   = "."
  show Loop    = "["
  show EndLoop = "]"

instance Read BFPrimitive where
  readsPrec _ ('>':xs) = [(Next, xs)]
  readsPrec _ ('<':xs) = [(Prev, xs)]
  readsPrec _ ('+':xs) = [(Inc, xs)]
  readsPrec _ ('-':xs) = [(Dec, xs)]
  readsPrec _ (',':xs) = [(Read, xs)]
  readsPrec _ ('.':xs) = [(Print, xs)]
  readsPrec _ ('[':xs) = [(Loop, xs)]
  readsPrec _ (']':xs) = [(EndLoop, xs)]
  readsPrec _ _        = []

  readList xs = case readsPrec 0 xs of
    []         -> [([], xs)]
    [(p, xs')] -> [(p : ps, xs'')]
      where [(ps, xs'')] = readList xs'

data BFSnippet = BFSnippet [BFPrimitive]
                 deriving (Eq)

instance Show BFSnippet where
  show (BFSnippet s) = concatMap show s
  
instance Read BFSnippet where
  readsPrec _ xs = [(BFSnippet s, xs')]
    where [(s, xs')] = readList xs
    
data BFAsmInstruction = Primitive BFPrimitive
                      | Dup
                      | Swap
                      | Target String
                      | Push Int
                      | Add
                      | Sub
                      | Mul
                      deriving (Eq)
                                                  
data AssemblyEnv = AssemblyEnv { 
  label :: String -> Int, 
  current :: Int }

newtype CodeGenerator a = Generator {
  runGenerator :: WriterT [BFPrimitive] (State AssemblyEnv) a
  } deriving (Monad, MonadWriter [BFPrimitive])

liftGenerator m = Generator (lift m)
getEnv   = liftGenerator get
putenv e = liftGenerator $ put e

runCodeGenerator s g = case runState (runWriterT (runGenerator g)) s of
  ((_, code), _) -> BFSnippet code


withAssembling w = BFSnippet ss  
  where (_, ss) = runWriter w 

prim x = tell [x]

next = prim Next

prev = prim Prev

dec = prim Dec

inc = prim Inc

loop w = do
  prim Loop
  w
  prim EndLoop

fromBF ss = tell $ read ss 

zero = fromBF "[-]"

dup = do
  copyTimes 2
  next; next; 
  copyTo (-2)
  prev


clearNext n = do
  replicateM_ n $ next >> zero
  replicateM_ n $ prev

copyTimes n = do
  let (go, ret) = if n >= 0 then (next, prev) else (prev, next)
  loop $ do
    dec
    replicateM_ (abs n) $ go >> inc
    replicateM_ (abs n) $ ret

copyTo n = do
  let (go, ret) = if n >= 0 then (next, prev) else (prev, next)
  loop $ do
    dec 
    replicateM_ (abs n) $ go
    inc
    replicateM_ (abs n) $ ret

emit (Primitive x) = prim x

emit Dup = do -- fromBF ">[-]>[-]<<[->+>+<<]>>[-<<+>>]<"
  clearNext 2
  dup
  
emit Swap = do -- fromBF ">[-]<[->+<]<[->+<]>>[-<<+>>]<"
  clearNext 1
  copyTimes 1
  prev
  copyTimes 1
  next; next
  copyTo (-2)
  prev

emit (Push i) = do 
  next
  zero
  fromBF $ replicate i '+'

emit (Target s) = do
  env <- getEnv
  emit . Push . label env $ s

emit Add = do
  copyTo (-1)
  prev

emit Sub = do 
  loop $ do
    dec
    prev
    dec
    next
  prev
  
emit Mul = do  
  clearNext 3
  copyTo 3
  prev
  copyTo 1
  dup
  dup
  next; next
  loop $ do
    dec
    prev
    prev
    copyTo (-2)
    prev
    dup
    next; next
  prev; prev; prev; prev
  
type BFAsmBlocks = Map.Map String [BFAsmInstruction]

allocateBlocks :: BFAsmBlocks -> String -> Int
allocateBlocks blocks = flip (Map.findWithDefault 0) $ labels
  where labels = Map.fromList $ zip (Map.keys blocks) [1..]
        
emitBlocks :: BFAsmBlocks -> [BFSnippet]
emitBlocks blocks = map emitBlock sortedBlocks
  where ids = allocateBlocks blocks 
        sortedBlocks = sortBy (comparing $ ids . fst) (Map.toList blocks)
        emitBlock (name, block) = runCodeGenerator (AssemblyEnv { label = ids, current = ids name }) $
                                  mapM_ emit block

main = putStrLn "Hello"
