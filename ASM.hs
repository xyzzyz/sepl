module ASM where

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


data BFASMInstruction = PopArg | Exit
                      | Pop | Push Int
                      | ASMInput | ASMOutput
                      | NextPos | PrevPos
                      | GetVar Int | SetVar Int
                      | SetArr Int | GetArrRef Int
                      | ASMNot | ASMOr | ASMAnd
                      | ASMAdd | ASMSub | ASMMul | ASMDiv | ASMMod
                      | ASMEquals | ASMLessThan | ASMGreaterThan
                      | ASMLessOrEqual | ASMGreaterOrEqual
                      | PutMarker
                      | AllocateFrame String [(String, String)] Int Int Int String
                      | DestroyFrame
                      | SetTarget String
                      | SetTargetIfThenElse String String
                      deriving (Eq, Show)

data BFSnippet = BFSnippet String [BFASMInstruction] [BFASMInstruction]
                 deriving (Eq, Show)
