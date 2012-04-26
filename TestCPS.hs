module Main where

import Control.Monad.State

import Text.ParserCombinators.Parsec
import Parser
import CPS

main = getContents >>=
       (\r ->
         case parse translationUnit "<stdin>" r of
           Left err -> putStrLn .show $ err
           Right res -> mapM_ (putStrLn . show . flip evalState (CPSEnv 0) . cpsTransformDef ) res)