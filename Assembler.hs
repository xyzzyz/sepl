module Assembler where

import Control.Monad.Writer
import Control.Monad.State

import Data.List
import Data.Char
import Data.Ord
import qualified Data.Map as Map

import ASM

data AssemblyEnv = AssemblyEnv {
  labels :: Map.Map String Int,
  curBlock :: String
}

data BFPrimitiveBlock = BFPrimitiveBlock [BFPrimitive]

instance Monoid BFPrimitiveBlock where
  mempty = BFPrimitiveBlock []
  mappend (BFPrimitiveBlock a) (BFPrimitiveBlock b) = BFPrimitiveBlock (mappend a b)

instance Show BFPrimitiveBlock where
  show (BFPrimitiveBlock a) = concatMap show a

toList :: BFPrimitiveBlock -> [BFPrimitive]
toList (BFPrimitiveBlock ss) = ss

type Assembler = WriterT BFPrimitiveBlock (State AssemblyEnv)

prepareAssemblyEnv :: [BFSnippet] -> AssemblyEnv
prepareAssemblyEnv snippets = AssemblyEnv ls "main"
  where isMainOrExit (BFSnippet "main" _ _) = True
        isMainOrExit (BFSnippet "$exit" _ _) = True
        isMainOrExit _ = False
        snippets' = filter (not . isMainOrExit) snippets
        ls = foldr addLabel (Map.fromList [("$exit", 2), ("main", 1)]) (zip snippets' [3..])
        addLabel (BFSnippet name _ _, n) m = Map.insert name n m

getEnv :: Assembler AssemblyEnv
getEnv = lift get

putEnv :: AssemblyEnv -> Assembler ()
putEnv = lift . put

computeLabel :: String -> Assembler Int
computeLabel name = do
  e <- getEnv
  let ls = labels e
      cur = ls Map.! (curBlock e)
      target = ls Map.! name
  if target > cur
    then return $ target - cur
    else return $ Map.size ls - cur + target

getLabel :: String -> Assembler Int
getLabel name = do
  e <- getEnv
  return $ (labels e) Map.! name

prim :: BFPrimitive -> Assembler ()
prim = tell . BFPrimitiveBlock . (: [])

next = prim Next
prev = prim Prev
inc = prim Inc
dec = prim Dec
loop :: Assembler a -> Assembler ()
loop w = do
  prim Loop
  w
  prim EndLoop

input = prim Read
output = prim Print


comment :: String -> Assembler ()
comment =  prim . Comment

cellWidth = 8

incBy :: Int -> Assembler ()
incBy n = replicateM_ n inc

decBy :: Int -> Assembler ()
decBy n = replicateM_ n dec


clear :: Assembler ()
clear = loop dec

clearMem :: Int -> Assembler ()
clearMem 0 = return ()
clearMem n | n > 0 = clear' (abs n) next
             | n < 0 = clear' (abs n) prev
  where clear' n forward = do
          clear
          replicateM_ (n-1) (forward >> clear)

move n | n == 0 = return ()
       | n > 0 = replicateM_ n next
       | n < 0 = replicateM_ (-n) prev

nextCell = move cellWidth
prevCell = move (- cellWidth)
pop = prevCell
copyTimesTo t n = loop $ do
  dec
  move n
  inc
  replicateM_ (t-1) $ next >> inc
  move (-(n+t-1))

newCell = do
  next
  clearMem cellWidth

dup = do
  copyTimesTo 2 (-2)
  prev; prev;
  copyTimesTo 1 2
  next; next

bfMul = do
  prevCell
  copyTimesTo 1 7
  nextCell
  loop $ do
    dec
    prev
    dup
    prev
    copyTimesTo 1 (-6)
    next; next
  pop

bfNot = do
  prev; inc; next
  loop $ do
    dec; prev; dec; next
  prev
  loop $ do
    dec; next; inc; prev
  next

goToMarker direction = do
  inc
  loop $ do
    dec
    direction
    inc
  dec

carryToMarker = do
  inc
  loop $ do
    dec
    move 6
    copyTimesTo 1 8
    move 2
    inc
  dec

carryBackToMarker = do
  inc
  loop $ do
    dec
    move 6
    copyTimesTo 1 (-8)
    move (-14)
    inc
  dec

carryThrough n = replicateM_ n carry

carry = do
  copyTimesTo 1 8
  move 8

assembleInstruction :: BFASMInstruction -> Assembler ()

assembleInstruction (Push n) = do
  newCell
  incBy n


assembleInstruction Pop = do
  pop

assembleInstruction ASMInput = do
  newCell
  input

assembleInstruction ASMOutput = do
  output

assembleInstruction NextPos = nextCell
assembleInstruction PrevPos = prevCell

assembleInstruction (GetVar i) = do
  comment $ "getvar" ++ show i
  putMarker
  prevCell
  move (-7)
  goToMarker prevCell
  move 7
  replicateM_ i nextCell
  dup
  move (-7)
  carryToMarker
  inc
  move 6
  copyTimesTo 1 1
  next

assembleInstruction (SetVar i) = do
  comment $ "setvar" ++ show i
  dup
  prev
  copyTimesTo 1 (-8)
  move (-6)
  dec -- put marker
  prevCell
  carryBackToMarker
  comment "carback"
  move 6
  carryThrough i
  next; clear; prev
  copyTimesTo 1 1
  next; next
  goToMarker nextCell
  inc
  move 7

assembleInstruction (SetTarget name) = do
  l <- computeLabel name
  comment $ "target" ++ name
  setTarget l

assembleInstruction (SetTargetIfThenElse thn els) = do
  thn' <- computeLabel thn
  els' <- computeLabel els
  prev; prev
  incBy thn'
  next
  incBy els'
  next
  loop $ do
    dec
    prev
    clear
    prev
    copyTimesTo 1 1
    next; next
  prev; prev;
  clear
  next; next
  inc

assembleInstruction ASMNot = bfNot


assembleInstruction ASMAnd = bfMul

assembleInstruction ASMOr = do
  bfNot
  prevCell
  bfNot
  nextCell
  bfMul
  bfNot

assembleInstruction ASMAdd = do
  copyTimesTo 1 (- cellWidth)
  pop

assembleInstruction ASMSub = do
  loop $ do
    dec
    prevCell
    dec
    nextCell
  pop

assembleInstruction ASMMul = bfMul

assembleInstruction PutMarker = putMarker

assembleInstruction (AllocateFrame name strings arrs locals args ret) = do
  putMarker
  prevCell
  move (-7)
  goToMarker prevCell
  move 7
  allocateStrings strings
  allocateArrays arrs
  allocateArgs args
  replicateM_ args clearMarker
  l <- getLabel ret
  comment $ "ret" ++ show l
  pushReturnAddress l
  next
  goToMarker nextCell -- frame marker
  nextCell
  goToMarker nextCell -- arrays marker
  move 7
  replicateM_ (arrs + args) nextCell
  comment "locals"
  replicateM_ locals newCell
  comment "endlocals"
  ln <- computeLabel name
  comment $ "target" ++ show ln
  setTarget ln

assembleInstruction DestroyFrame = do
  comment "destroy"
  copyTimesTo 1 (-1)
  move (-7)
  carryBackToMarker -- array marker
  inc
  comment "carried"
  carryBackToMarker -- frame marker
  inc
  carryBackToMarker -- call marker
  comment "got to call marker"
  inc
  move 6
  copyTimesTo 1 (-3)
  next
  copyTimesTo 1 (-1)
  prev
  e <- getEnv
  let cur = (labels e) Map.! (curBlock e)
  comment $ "cur" ++ curBlock e
  decBy cur
  next
  comment "retq"
  inc

assembleInstruction PopArg = do
  comment "poparg"
  clear
  move (-4)
  copyTimesTo 1 4
  move 4

assembleInstruction Exit = do
  newCell

assembleInstruction i = comment . ("Not implemented: " ++) . show $ i

putMarker = do
  newCell
  move (-7)
  dec
  move 7

clearMarker = do
  move (-7)
  inc
  prev

allocateStrings strings = do
  nextCell
  move (-7)
  goToMarker nextCell
  move 7
  -- put strings here
  putMarker
  prevCell; prevCell
  move (-7)
  goToMarker prevCell
  move 7

allocateArrays arrs = replicateM_ arrs putMarker
allocateArgs args = allocateArg 0
  where allocateArg n | n == args = return ()
                      | otherwise = do
          comment "allocarg"
          nextCell
          copyTimesTo 1 (-1) -- prepare to carry
          move (-7)
          carryToMarker -- next frame
          comment "carriedthroughmarker"
          move 6
          carryThrough 1
          move (-6)
          carryToMarker -- after arrays
          move 6
          comment "throughargs"
          carryThrough n -- carry through args
          next
          newCell
          prevCell; prev -- move to carried value
          copyTimesTo 1 9 -- and copy it
          comment "arginplace"
          move (-6) -- move to marker pos
          goToMarker prevCell -- arrays marker
          prevCell
          goToMarker prevCell -- frame marker
          prevCell
          goToMarker prevCell -- arg marker
          move 7
          putMarker -- put marker over allocated arg
          allocateArg (n+1)

allocateLocals locals = return ()
pushReturnAddress ret = do
  e <- getEnv
  let addr = ret + Map.size (labels e)
  comment ("pushaddr$" ++ show ret ++ "$" ++ show addr)
  incBy addr

setTarget l = do
  newCell
  inc
  prev
  incBy l
  next


traceAssembler :: BFASMInstruction -> Assembler BFASMInstruction
traceAssembler i = do
  -- comment . (++ "...") . show $ i
  return i

assemble :: BFSnippet -> Assembler String
assemble (BFSnippet name body jump) = do
  e <- getEnv
  putEnv $ e { curBlock = name }

  mapM_ assemble' body
  mapM_ assemble' jump
  return name
  where assemble' = (assembleInstruction =<<) . traceAssembler

actAndClear :: Assembler a -> Assembler (a, BFPrimitiveBlock)
actAndClear act = censor (const (BFPrimitiveBlock [])) (listen act)

assembleSnippet :: BFSnippet -> Assembler (String, BFPrimitiveBlock)
assembleSnippet s = actAndClear (assemble s)


preamble :: Assembler ()
preamble = do
  e <- getEnv
  putMarker
  incBy (Map.size (labels e) + 2)
  putMarker
  putMarker
  setTarget 1

epilogue :: Assembler ()
epilogue = return ()

wrapBlocks' :: [(String, BFPrimitiveBlock)] -> Assembler ()
wrapBlocks' ss = do
  preamble
  loop $ do
    mapM_ putBlock ss
  epilogue
  where putBlock (n, b) = do
          loop $ do
            prev
            dec
            loop $ do
              prev; prev
            next
            loop $ do
              dec; tell b; prev; prev
          next; next;

wrapBlocks :: [(String, BFPrimitiveBlock)] -> Assembler ()
wrapBlocks ss = do
  env <- getEnv
  let ss' = sortBy (comparing ((labels env Map.!) . fst)) ss
  wrapBlocks' ss'



assembleSnippets :: [BFSnippet] -> Assembler ()
assembleSnippets ss = do
  ss' <- mapM assembleSnippet (BFSnippet "$exit" [] [Push 0, Exit] : ss)
  wrapBlocks ss'

generateAssemblyFromSnippets :: [BFSnippet] -> BFPrimitiveBlock
generateAssemblyFromSnippets ss = asm
  where ((_, asm), _) = runState (runWriterT (assembleSnippets ss)) env
        env = prepareAssemblyEnv ss