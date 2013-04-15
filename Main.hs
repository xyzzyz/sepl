module Main where

import System.IO
import System.Environment

import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Text.Show.Pretty

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
  liftIO (hPutStrLn stderr err)
  fail ""

printDebug :: String -> CompilerProgram ()
printDebug = liftIO . hPutStrLn stderr

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
  printDebug $ "PARSED CORRECTLY:\n\n" ++ ppShow unit ++ "\n\n\n"

  typeCheck unit
  printDebug $ "TYPE CHECKING PASSED\n\n\n"

  return unit

execute :: (TranslationUnit -> CompilerProgram ()) -> String -> CompilerProgram ()
execute uh input = do
  unit <- parseAndTypecheck input
  uh unit
  return ()

compiler :: TranslationUnit -> CompilerProgram ()
compiler unit = do
  let cpsBlocks = cpsTransformTranslationUnit unit
      snippets = generateFunsCode cpsBlocks
  printDebug $ "CPS BLOCKS:\n\n" ++ ppShow cpsBlocks ++ "\n\n\n"
  printDebug $ "BFSNIPPETS:\n\n" ++ ppShow snippets ++ "\n\n\n"

  let primitiveBlocks = generateAssemblyFromSnippets snippets
  printDebug $ ppShow primitiveBlocks
  return ()

interpreter :: TranslationUnit -> CompilerProgram ()
interpreter unit = do
  liftIO (evalTranslationUnit unit)
  return ()

main :: IO ()
main = do
  args <- getArgs
  input <- getContents
  if "-c" `elem` args
    then runMaybeT (execute compiler input)
    else runMaybeT (execute interpreter input)
  return ()
