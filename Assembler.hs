{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Assembler(BFPrimitive(..), BFSnippet(..), BFAsmInstruction(..), AssemblyEnv(..),
                 runCodeGenerator,
                 prim, next, prev, dec, inc, loop,
                 fromBF, zero, clearNext, copyTimesTo,
                 emit,
                 allocateBlocks, emitBlocks, wrapSnippets) where

import Control.Monad.Writer
import Control.Monad.State

import Data.List
import Data.Ord
import qualified Data.Map as Map


data BFPrimitive = Next | Prev | Inc | Dec | Read | Print | Loop | EndLoop | Comment String
                 deriving (Eq)

instance Show BFPrimitive where
  show Next        = ">"
  show Prev        = "<"
  show Inc         = "+"
  show Dec         = "-"
  show Read        = ","
  show Print       = "."
  show Loop        = "["
  show EndLoop     = "]"
  show (Comment s) = "\n" ++ s ++ "\n"

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
                      | DupX Int
                      | Swap
                      | Target String
                      | ReturnTo String
                      | Return
                      | Push Int
                      | Add
                      | Sub
                      | Mul
                      deriving (Eq)

data AssemblyEnv = AssemblyEnv {
  label :: String -> Int,
  current :: Int,
  count :: Int}

newtype CodeGenerator a = Generator {
  runGenerator :: WriterT [BFPrimitive] (State AssemblyEnv) a
  } deriving (Monad, MonadWriter [BFPrimitive])

liftGenerator m = Generator (lift m)
getEnv   = liftGenerator get
putEnv e = liftGenerator $ put e


getLabel s = do
  env <- getEnv
  return $ (label env) s

getCurrent = do
  env <- getEnv
  return $ current env

getCount = do
  env <- getEnv
  return $ count env

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

comment s = prim (Comment s)

fromBF ss = tell $ read ss 

zero = fromBF "[-]"

move n = replicateM_ (abs n) (if n > 0 then next else prev)

dup n = do
  move (-n)
  copyTimesTo 2 (n+1)
  move (n+2)
  copyTimesTo 1 (-n-2)
  prev

push i = fromBF $ replicate i '+'

clearNext n = do
  replicateM_ n $ next >> zero
  replicateM_ n $ prev

copyTimesTo t n = loop $ do
  dec
  move n
  inc
  replicateM_ (t-1) $ next >> inc
  move (-(n+t-1))

emit (Primitive x) = prim x

emit (DupX n) = do -- fromBF ">[-]>[-]<<[->+>+<<]>>[-<<+>>]<"
  clearNext 2
  dup n

emit Swap = do -- fromBF ">[-]<[->+<]<[->+<]>>[-<<+>>]<"
  clearNext 1
  copyTimesTo 1 1
  prev
  copyTimesTo 1 1
  next; next
  copyTimesTo 1 (-2)
  prev

emit (Push i) = do
  next
  zero
  push i

emit (Target s) = do
  i <- getLabel s
  cur <- getCurrent
  count <- getCount
  next; zero
  if i == cur
    then push count
    else push $ (i - cur) `mod` count

emit (ReturnTo s) = do
  i <- getLabel s
  next
  zero
  push i

emit Return = do
  cur <- getCurrent
  copyTimesTo 1 (-2)
  prev
  clearNext 3
  fromBF $ replicate cur '+'

emit Add = do
  copyTimesTo 1 (-1)
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
  copyTimesTo 1 3
  prev
  copyTimesTo 1 1
  dup 0
  dup 0
  next; next
  loop $ do
    dec
    prev
    prev
    copyTimesTo 1 (-2)
    prev
    dup 0
    next; next
  prev; prev; prev; prev
type BFAsmBlocks = Map.Map String [BFAsmInstruction]

allocateBlocks :: BFAsmBlocks -> String -> Int
allocateBlocks blocks = flip (Map.findWithDefault 0) $ labels
  where labels = Map.fromList $ [("exit", 1)] ++ zip (Map.keys . Map.delete "exit" $ blocks) [2..]

emitBlocks :: BFAsmBlocks -> [BFSnippet]
emitBlocks blocks = map emitBlock sortedBlocks
  where ids = allocateBlocks blocks
        sortedBlocks = sortBy (comparing $ ids . fst) (Map.toList blocks)
        emitBlock (name, block) = runCodeGenerator (AssemblyEnv { label = ids,
                                                                  current = ids name,
                                                                  count = length sortedBlocks }) $
                                  comment name >> wrap block
        wrap block = do
          clearNext 1
          next; inc; next; inc
          prev; prev
          dec
          loop $ do
            next; next;
            dec
          next
          comment "actual block"
          loop $ do
            dec
            next; dec
            prev; prev; prev
            mapM_ emit block
            clearNext 3
            next; next; next
          prev; prev; prev

wrapSnippets :: [BFSnippet] -> BFSnippet
wrapSnippets ss = BFSnippet $ [Next, Inc, Loop, Inc] ++ concatMap snippetToCode ss ++ [Dec, EndLoop]
  where snippetToCode (BFSnippet s) = s

main = putStrLn "Hello"
