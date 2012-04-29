module Main where

import Text.ParserCombinators.Parsec

import System.IO
import System.Directory
import System.FilePath
import System.Cmd
import System.Exit

import Text.Printf

import Test.HUnit

import CPS
import AST
import Parser
import TypeChecker
import CodeGenerator
import Interpreter

testsDirectory :: String
testsDirectory = "tests/"

getFilesWithExtension :: String -> IO [FilePath]
getFilesWithExtension ext = do
  dir <- getDirectoryContents testsDirectory
  return $ filter ((ext ==) . takeExtension) dir

getPreprocessFiles :: IO [FilePath]
getPreprocessFiles = getFilesWithExtension ".in"

preprocessFile :: FilePath -> IO ExitCode
preprocessFile name = system $ printf "cpp -P %s/%s %s/%s" path name path (dropExtension name)
  where path = testsDirectory

getTestFiles :: IO [String]
getTestFiles = getFilesWithExtension ".bf"

parseTestCase :: SourceName -> String -> Either String TranslationUnit
parseTestCase filePath program =
  case parse translationUnit filePath program of
    Left err -> Left (show err)
    Right unit -> Right unit

typeCheckTestCase :: TranslationUnit -> Either String Bool
typeCheckTestCase translationUnit =
  case typeCheckTranslationUnit translationUnit of
    Left err -> Left (show err)
    Right _ -> Right True

getProgramContents :: FilePath -> IO String
getProgramContents filePath = do
  handle <- openFile (combine testsDirectory filePath) ReadMode
  hGetContents handle

testCase :: SourceName -> String -> Either String TranslationUnit
testCase filePath program = do
  unit <- parseTestCase filePath program
  typeCheckTestCase unit
  return unit

assertEither (Right _)  = assertString ""
assertEither (Left str) = assertString str

makeTestCase filePath = do
  program <- getProgramContents filePath
  let eith = testCase filePath program
  putStrLn $ "--- " ++ filePath ++ ":"
  case eith of
    Left _ -> return ()
    Right unit -> evalTranslationUnit unit >> return ()
  putStrLn $ "--- " ++ filePath ++ " DONE"
  return $ TestLabel testName (TestCase (assertEither eith))
    where testName = takeFileName filePath

getCases filePaths = do
  cases <- mapM makeTestCase filePaths
  return $ TestList cases

main = do
  preprocessPaths <- getPreprocessFiles
  mapM_ preprocessFile preprocessPaths
  filePaths <- getTestFiles
  testCases <- getCases filePaths
  runTestTT testCases
  return ()
