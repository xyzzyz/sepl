import Test.HUnit
import Assembler

import qualified Data.Map as Map

emptyEnv = AssemblyEnv { label = error, current = 0, count = 0}

testDup = "testDup" ~: "emit Dup" ~: 
          (runCodeGenerator emptyEnv $ emit (DupX 0))
          ~=? 
          (runCodeGenerator emptyEnv $ fromBF ">[-]>[-]<<[->+>+<<]>>[-<<+>>]<")
          
testSwap = "testSwap" ~: "emit Swap" ~: 
          (runCodeGenerator emptyEnv $ emit Swap) 
          ~=? 
          (runCodeGenerator emptyEnv $ fromBF ">[-]<[->+<]<[->+<]>>[-<<+>>]<")
          
          
blocks = Map.fromList [
  ("exit", []),
  ("first", [ReturnTo "second", Push 3, DupX 0, DupX 0, Add, Target "third"]),
  ("second", [Push 5, Add, Target "exit"]),
  ("third", [Mul, Return])
  ]

tests = test [testDup, testSwap]

main = runTestTT tests