{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)
import Data.Void

import Lilyval

type Parser = Parsec Void Text

-- pitchClassP :: Parser PitchClass
-- pitchClassP =  do
--     oneOf "abcdef"
