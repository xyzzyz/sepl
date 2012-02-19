import Test.HUnit
import Assembler

import qualified Data.Map as Map

emptyEnv = AssemblyEnv { label = error, current = 0, count = 0}

testDup = "testDup" ~: "emit Dup" ~: 
          (runCodeGenerator emptyEnv $ emit Dup) 
          ~=? 
          (runCodeGenerator emptyEnv $ fromBF ">[-]>[-]<<[->+>+<<]>>[-<<+>>]<")
          
testSwap = "testSwap" ~: "emit Swap" ~: 
          (runCodeGenerator emptyEnv $ emit Swap) 
          ~=? 
          (runCodeGenerator emptyEnv $ fromBF ">[-]<[->+<]<[->+<]>>[-<<+>>]<")
          
          
blocks = Map.fromList [
  ("exit", []),
  ("first", [Push 2, Push 3, Mul, Target "third"]),
  ("second", []),
  ("third", [Push 5, Dup, Add, Target "exit"]),
  ("fourth", [])
  ]

tests = test [testDup, testSwap]

main = runTestTT tests