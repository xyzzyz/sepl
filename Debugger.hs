{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.Writer

import Graphics.Vty
import Graphics.Vty.Inline

import Data.Array
import Data.Maybe

import ASM

data BFAST = BFPrim BFPrimitive | BFLoop [BFAST] | BFMain [BFAST]
           deriving (Show)

bfs = "><+-,.[]"
bfMap = [('>', Next), ('<', Prev), ('+', Inc), ('-', Dec),
         (',', Read), ('.', Print), ('[', Loop), (']', EndLoop)]

parseBF [] = return []
parseBF (x:xs)
  | x `elem` bfs = do
    case fromJust (x `lookup` bfMap) of
      EndLoop -> return xs
      Loop -> do
        (rest, bs) <- listen (parseBF xs)
        tell [BFLoop bs]
        parseBF rest
      b -> do
        tell [BFPrim b]
        parseBF xs
  | otherwise = parseBF xs

data InterpreterState = InterpreterState {
  _mem :: Array Int Int,
  _pos :: Int
  }

$( makeLenses ''InterpreterState )



main = do
  input <- getContents
  let code = BFMain . execWriter . parseBF $ input
  putStrLn (show code)
  -- term <- terminal_handle
  -- putStr "Not styled. "
  -- put_attr_change term $ do
  --   back_color red
  --   apply_style underline
  -- putStr " Styled! "
  -- put_attr_change term $ default_all
  -- putStrLn "Not styled."
  -- release_terminal term
