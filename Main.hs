module Main where

import System.IO
import System.Environment

import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import qualified Text.ParserCombinators.Parsec as Parsec

import AST
import Parser
import TypeChecker
import CPS
import CodeGenerator
import Assembler
import Interpreter

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

parseAndTypecheck :: String -> CompilerProgram TranslationUnit
parseAndTypecheck input = do
  unit <- parse input
  lift (hPutStrLn stderr ("PARSED CORRECTLY:\n\n" ++ show unit ++ "\n\n\n"))

  typeCheck unit
  lift (hPutStrLn stderr "TYPE CHECKING PASSED\n\n\n")
  
  return unit

execute :: (TranslationUnit -> CompilerProgram ()) -> String -> CompilerProgram ()
execute uh input = do
  unit <- parseAndTypecheck input
  uh unit
  return ()

compilator :: TranslationUnit -> CompilerProgram ()
compilator unit = do
  let cpsBlocks = cpsTransformTranslationUnit unit
      snippets = generateFunsCode cpsBlocks
  lift (hPutStrLn stderr ("CPS BLOCKS:\n\n" ++ show cpsBlocks ++ "\n\n\n"))
  lift (hPutStrLn stderr ("BFSNIPPETS:\n\n" ++ show snippets ++ "\n\n\n"))

  let primitiveBlocks = generateAssemblyFromSnippets snippets
  lift (putStrLn (show primitiveBlocks))
  return ()

interpreter :: TranslationUnit -> CompilerProgram ()
interpreter unit = do
  lift (evalTranslationUnit unit)
  return ()

main :: IO ()
main = do
  args <- getArgs
  input <- getContents
  if "-c" `elem` args
    then runMaybeT (execute compilator input)
    else runMaybeT (execute interpreter input)
  return ()
