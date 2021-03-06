module Main where

import Text.ParserCombinators.Parsec
import Parser

main = getContents >>= 
       (\r -> 
         case parse translationUnit "<stdin>" r of
           Left err -> putStrLn .show $ err
           Right res -> mapM_ (putStrLn . show) $ res)