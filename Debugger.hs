{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent
import Control.Applicative hiding ((<|>))
import Control.Lens
import Control.Monad.State

import Graphics.Vty
import Graphics.Vty.Inline

import qualified Data.Sequence as S
import Data.Maybe
import Data.Foldable
import Data.Char

import Prelude hiding (foldr, foldl)

import ASM


data InterpreterState = InterpreterState {
  _mem :: S.Seq Int,
  _memPos :: Int,
  _code :: S.Seq BFPrimitive,
  _codePos :: Int,
  _plainCode :: String
  }

$( makeLenses ''InterpreterState )

type Interpreter = StateT InterpreterState IO

memSize = 20

makeInterpreter input = InterpreterState (S.replicate memSize 0) 0 (S.fromList . read $ input) 0 input


step :: Interpreter Bool
step = do
  instr <- S.index <$> use code <*> use codePos
  case instr of
    Inc -> do
      pos <- use memPos
      mem %= S.adjust ((+) 1) pos
    Dec -> do
      pos <- use memPos
      mem %= S.adjust (flip (-) 1) pos
    Next -> memPos += 1
    Prev -> memPos -= 1
    Read -> do
      c <- liftIO getChar
      pos <- use memPos
      mem %= S.update pos (ord c)
    Print -> do
      pos <- use memPos
      c <- use (mem . singular (ix pos))
      liftIO . putChar . chr $ c
    Loop -> do
      pos <- use memPos
      m <- use mem
      when ((m `S.index` pos) == 0) $ do
        c <- use code
        cpos <- use codePos
        codePos .= findMatchingPos EndLoop Loop 1 c cpos 0
    EndLoop -> do
        c <- use code
        cpos <- use codePos
        codePos .= findMatchingPos Loop EndLoop (-1) c cpos 0 - 1

  codePos += 1
  (==) <$> use codePos <*> (S.length <$> use code) -- return True if program ended
    where
      findMatchingPos :: BFPrimitive -> BFPrimitive -> Int -> S.Seq BFPrimitive -> Int -> Int -> Int
      findMatchingPos to from dir code pos stack
            | (code `S.index` pos) == to && stack == 1 = pos
            | (code `S.index` pos) == to =
                findMatchingPos to from dir code (pos + dir) (stack-1)
            | (code `S.index` pos) == from =
                findMatchingPos to from dir code (pos + dir) (stack+1)
            | otherwise = findMatchingPos to from dir code (pos + dir) stack

cellWidth = 5

codeImage :: Interpreter Image
codeImage = do
  c <- use plainCode
  pos <- use codePos
  return $ string def_attr c <-> (string def_attr (replicate pos ' ') <|> char def_attr '^')

memImage :: Interpreter Image
memImage = do
  m <- use mem
  pos <- use memPos
  return $ makeMemView m pos
  where side = char def_attr '+' <-> char def_attr '|' <-> char def_attr '+'
        cells m = foldr makeMemRow side m
        makeCell :: Int -> Image
        makeCell n = string def_attr (replicate cellWidth '-') <->
                     string def_attr (show n) <->
                     string def_attr (replicate cellWidth '-')
        makeMemRow n rest = side <|> makeCell n <|> rest
        makePointerRow pos = string def_attr (replicate (1 + pos*(1+cellWidth)) ' ') <|>
                             char def_attr '^'
        makeMemView mem pos = cells mem <-> makePointerRow pos

runInterpreter :: TerminalHandle -> Interpreter ()
runInterpreter term = do
  bounds <- display_bounds term
  context <- display_context term bounds
  codeImg <- codeImage
  memImg <- memImage
  end <- step
  output_picture context . pic_for_image $ codeImg <-> memImg
  liftIO $ threadDelay (1000*200)
  unless end $
    runInterpreter term


main = do
  input <- getContents
  term <- terminal_handle
  reserve_display term
  execStateT (runInterpreter term) (makeInterpreter input)
  release_display term
  release_terminal term
