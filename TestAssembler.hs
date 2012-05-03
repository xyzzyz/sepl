import Data.Char

import Assembler
import ASM
pushChar = Push . ord

sampleSnippets = [
  BFSnippet "main" [Push 3, Push 5, ASMMul, Pop] [SetTarget "$exit"]]


sampleSnippets2 = [
  BFSnippet "main" [] [PutMarker, AllocateFrame "hello" [] 0 0 0 "foobar"],
  BFSnippet "foobar" [PopArg, Pop, pushChar 'Q', ASMOutput, Pop] [SetTarget "$exit"],
  BFSnippet "hello" [pushChar 'h', ASMOutput, Pop,
                     pushChar 'e', ASMOutput, Pop] [Push 2, DestroyFrame]
  ]

sampleSnippets3 = [
  BFSnippet "main" [] [PutMarker,Push 2,Push 1,AllocateFrame "foo" [] 0 1 2 "$main$pop0"],
  BFSnippet "$main$pop0" [PopArg,Pop] [SetTarget "$main$after_call1"],
  BFSnippet "$main$after_call1" [Push 111,ASMOutput,Pop] [Push 0,DestroyFrame],
  BFSnippet "foo" [Push 108,ASMOutput,Pop] [Push 2,DestroyFrame]
  ]
