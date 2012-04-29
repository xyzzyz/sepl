module Main where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import qualified Text.ParserCombinators.Parsec as Parsec

import Parser
import AST
import TypeChecker
import CPS
import CodeGenerator

type CompilerProgram = MaybeT IO

reportError :: String -> CompilerProgram a
reportError err = do
  lift (putStrLn err)
  fail ""

parse :: String -> CompilerProgram TranslationUnit
parse input = case Parsec.parse translationUnit "<stdin>" input of
  Left err -> reportError (show err)
  Right res -> return res

typeCheck :: TranslationUnit -> CompilerProgram ()
typeCheck unit = case typeCheckTranslationUnit unit of
  Left err -> reportError (show err)
  Right _ -> return ()

compile :: String -> CompilerProgram ()
compile input = do
  unit <- parse input
  lift (putStrLn ("PARSED CORRECTLY:\n\n" ++ show unit ++ "\n\n\n"))
  typeCheck unit
  lift (putStrLn "TYPE CHECKING PASSED\n\n\n")
  let cpsBlocks = cpsTransformTranslationUnit unit
      funs = generateFunsCode cpsBlocks
  lift (putStrLn ("CPS BLOCKS:\n\n" ++ show cpsBlocks ++ "\n\n\n"))
  lift (putStrLn ("BFSNIPPETS:\n\n" ++ show funs ++ "\n\n\n"))

  return ()

main :: IO ()
main = do
  input <- getContents
  result <- runMaybeT (compile input)
  case result of
    Just () -> putStrLn "Everything all right"
    Nothing -> putStrLn "Errors, stopped"