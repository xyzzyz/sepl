module Assembler where

import Control.Monad.Writer

data BFPrimitive = Next | Prev | Inc | Dec | Read | Print | Loop | EndLoop

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
    []         -> []
    [(p, xs')] -> p : readList xs'

data BFSnippet = BFSnippet [BFPrimitive]

instance Show BFSnippet where
  show (BFSnippet s) = concatMap show s
  
instance Read BFSnippet where
  readsPrec _ xs = [(BFSnippet s, xs')]
    where [(s, xs')] = readList xs
    
data BFAsmInstruction = Primitive BFPrimitive
                      | Dup
                      | Swap
                        
data AssemblyEnv = AssemblyEnv { label :: String -> Int }
                   
withAssembling :: Writer [BFPrimitive] a -> BFSnippet
withAssembling w = BFSnippet ss  
  where (_, ss) = runWriter w 

prim x = tell [x]

emit (Primitive x) = prim x
emit Dup = fromBF ">[-]>[-]<<[->+>+<<]>>[-<<+>>]<"
  
fromBF :: String -> Writer [BFPrimitive] ()
fromBF ss = tell $ read ss 



main = putStrLn "Hello"

  