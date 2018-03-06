{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (void, foldM)
import Data.Void
import qualified Control.Monad.State as S
import           Control.Monad.Trans

import Lilyval
import Parser

main :: IO ()
main = do
  cmajor <- T.readFile "./examples/c_major_simple.txt"
  print $ S.evalStateT 
            (runParserT fileP "./examples/c_major_simple.txt" cmajor) defaultState
  -- print $ S.runStateT (runParserT fileP "" "#key fsm\n%#time 5/4\n") defaultState



keyTest1 = "#key am\n"
keyTest2 = "#key cM\n"
keyTest3 = "#key dsm\n"
keyTest4 = "#k bm\n"

timeTest1 = "#time 4/4\n"
timeTest2 = "#time 3/8\n"
timeTest3 = "#time 4/5\n"
timeTest4 = "time 4/4\n"

tempoTest1 = "#tempo 4,=60\n"
tempoTest2 = "#tempo 8=200\n"
tempoTest3 = "#tempo 16=12\n"
tempoTest4 = "#tempo 2 = 80\n"

primTest1 = "#tempo 4=60\na4_4" :: Text
primTest2 = "#tempo 4=60\nr4" :: Text
