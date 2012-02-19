import Test.HUnit
import Assembler


emptyEnv = AssemblyEnv { label = error, current = 0 }

testDup = "testDup" ~: "emit Dup" ~: 
          (runCodeGenerator emptyEnv $ emit Dup) 
          ~=? 
          (runCodeGenerator emptyEnv $ fromBF ">[-]>[-]<<[->+>+<<]>>[-<<+>>]<")
          
testSwap = "testSwap" ~: "emit Swap" ~: 
          (runCodeGenerator emptyEnv $ emit Swap) 
          ~=? 
          (runCodeGenerator emptyEnv $ fromBF ">[-]<[->+<]<[->+<]>>[-<<+>>]<")
          
tests = test [testDup, testSwap]

main = runTestTT tests