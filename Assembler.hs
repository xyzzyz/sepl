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
    []         -> [([], xs)]
    [(p, xs')] -> [(p : ps, xs'')]
      where [(ps, xs'')] = readList xs'

data BFSnippet = BFSnippet [BFPrimitive]

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
                        
data AssemblyEnv = AssemblyEnv { 
  label :: String -> Int, 
  current :: Int }

withAssembling :: Writer [BFPrimitive] a -> BFSnippet
withAssembling w = BFSnippet ss  
  where (_, ss) = runWriter w 

prim :: BFPrimitive -> Writer [BFPrimitive] ()
prim x = tell [x]

next :: Writer [BFPrimitive] ()
next = prim Next

prev :: Writer [BFPrimitive] ()
prev = prim Prev

dec :: Writer [BFPrimitive] ()
dec = prim Dec

inc :: Writer [BFPrimitive] ()
inc = prim Inc

loop :: Writer [BFPrimitive] a -> Writer [BFPrimitive] ()
loop w = do
  prim Loop
  w
  prim EndLoop

fromBF :: String -> Writer [BFPrimitive] ()
fromBF ss = tell $ read ss 

zero :: Writer [BFPrimitive] ()
zero = fromBF "[-]"

copyTimes :: Int -> Writer [BFPrimitive] ()
copyTimes n = do
  let (go, ret) = if n >= 0 then (next, prev) else (prev, next)
  loop $ do
    dec
    replicateM_ (abs n) $ go >> inc
    replicateM_ (abs n) $ ret

copyTo :: Int -> Writer [BFPrimitive] ()
copyTo n = do
  let (go, ret) = if n >= 0 then (next, prev) else (prev, next)
  loop $ do
    dec 
    replicateM_ (abs n) $ go
    inc
    replicateM_ (abs n) $ ret

emit (Primitive x) = prim x

emit Dup = do -- fromBF ">[-]>[-]<<[->+>+<<]>>[-<<+>>]<"
  next; zero
  next; zero
  prev; prev
  copyTimes 2
  next; next; 
  copyTo (-2)
  prev

emit Swap = do -- fromBF ">[-]<[->+<]<[->+<]>>[-<<+>>]<"
  next; zero
  prev
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




main = putStrLn "Hello"

  