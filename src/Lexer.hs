{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)
import Data.Void

import Lilyval

{- 
 - Many examples taken from 
 - https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
 -}
type Parser = Parsec Void Text

-- define space consumer
spaceConsumer :: Parser ()
spaceConsumer = L.space exceptNewlines lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "%"
    blockCmnt = empty
    -- newLines mean the end of a measure, so they are syntactically relevant
    exceptNewlines = skipSome $ oneOf ['\t', ' ', '\v']

-- This consumes all whitespace after every lexeme, but not before
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- Augment the symbol parser to consume whitespace after a fixed symbol
symbol :: Text -> Parser Text 
symbol = L.symbol spaceConsumer

-- Parse the # symbol that denotes a key, time sig, or tempo
