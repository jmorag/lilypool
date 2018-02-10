{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void, foldM)
import Data.Void

import Lilyval
import Parser

main :: IO ()
main = do

  -- parseTest keyP keyTest1
  -- parseTest keyP keyTest2
  -- parseTest keyP keyTest3
  -- -- parseTest keyP keyTest4
  -- parseTest timeP timeTest1
  -- parseTest timeP timeTest2
  -- -- parseTest timeP timeTest3
  -- -- parseTest timeP timeTest4
  -- parseTest tempoP tempoTest1
  -- parseTest tempoP tempoTest2
  -- parseTest tempoP tempoTest3
  -- parseTest tempoP tempoTest4
  
  -- parseTest noteP "c4_4"
  parseTest lP ""
  return ()



lookaheadtest = ","
keyTest1 = "#key am\n"
keyTest2 = "#key cM\n"
keyTest3 = "#key dsm\n"
keyTest4 = "#k bm\n"

timeTest1 = "#time 4/4\n"
timeTest2 = "#time 3/8\n"
timeTest3 = "#time 4/5\n"
timeTest4 = "time 4/4\n"

tempoTest1 = "#tempo 4=60\n"
tempoTest2 = "#tempo 8=200\n"
tempoTest3 = "#tempo 1=12\n"
tempoTest4 = "#tempo 2 = 80\n"


