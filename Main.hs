module Main where

import System.IO

import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import qualified Text.ParserCombinators.Parsec as Parsec

import AST
import Parser
import TypeChecker
import CPS
import CodeGenerator
import Assembler

type CompilerProgram = MaybeT IO

reportError :: String -> CompilerProgram a
reportError err = do
  lift (hPutStrLn stderr err)
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
  lift (hPutStrLn stderr ("PARSED CORRECTLY:\n\n" ++ show unit ++ "\n\n\n"))

  typeCheck unit
  lift (hPutStrLn stderr "TYPE CHECKING PASSED\n\n\n")

  let cpsBlocks = cpsTransformTranslationUnit unit
      snippets = generateFunsCode cpsBlocks
  lift (hPutStrLn stderr ("CPS BLOCKS:\n\n" ++ show cpsBlocks ++ "\n\n\n"))
  lift (hPutStrLn stderr ("BFSNIPPETS:\n\n" ++ show snippets ++ "\n\n\n"))

  let primitiveBlocks = generateAssemblyFromSnippets snippets
  lift (putStrLn (show primitiveBlocks))
  return ()

main :: IO ()
main = do
  input <- getContents
  result <- runMaybeT (compile input)
  case result of
    Just () -> hPutStrLn stderr "Everything all right"
    Nothing -> hPutStrLn stderr "Errors, stopped"
