module Main where

import Control.Monad.State

import Text.ParserCombinators.Parsec
import Parser
import CPS
import CodeGenerator

main = getContents >>=
       (\r ->
         case parse translationUnit "<stdin>" r of
           Left err -> putStrLn .show $ err
           Right res -> putStrLn . show .  generateFunsCode . cpsTransformTranslationUnit $ res)