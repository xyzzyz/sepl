module Main where

import Text.ParserCombinators.Parsec

import System.IO
import System.Directory
import System.FilePath

import Test.HUnit

import Parser
import TypeChecker

testsDirectory = "tests/"

getTestFiles = do
  dir <- getDirectoryContents testsDirectory
  return $ filter ((".bf" ==) . takeExtension) dir

parseTestCase filePath program =
  case parse translationUnit filePath program of
    Left err -> Left (show err)
    Right unit -> Right unit

typeCheckTestCase translationUnit =
  case typeCheckTranslationUnit translationUnit of
    Left err -> Left (show err)
    Right _ -> Right True

getProgramContents filePath = do
  handle <- openFile (combine testsDirectory filePath) ReadMode
  hGetContents handle

testCase filePath program = do
  unit <- parseTestCase filePath program
  typeCheckTestCase unit

assertEither (Right _)  = assertString ""
assertEither (Left str) = assertString str

makeTestCase filePath = do
  program <- getProgramContents filePath
  return $ TestLabel testName (TestCase (assertEither
                                         (testCase filePath program)))
    where testName = takeFileName filePath

getCases filePaths = do
  cases <- mapM makeTestCase filePaths
  return $ TestList cases

main = do
  filePaths <- getTestFiles
  testCases <- getCases filePaths
  runTestTT testCases
  return ()
