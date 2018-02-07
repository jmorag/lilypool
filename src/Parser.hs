{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void, foldM)
import Data.Void

import           Data.Array (listArray, (!))
import           Data.Maybe (fromMaybe)

import Lilyval

type Parser = Parsec Void Text

{- 
 - Many examples taken from 
 - https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
 -}

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

-- Parse the reserved time, key, and tempo symbols
keyP :: Parser Key
keyP = do 
    symbol "#key"
    pitchClass <- pitchClassP
    quality <- qualityP
    spaceConsumer
    eol
    return $ Key pitchClass quality

qualityP :: Parser Quality
qualityP = do
    qualityName <- char' 'm'
    case qualityName of
        'm' -> return Minor
        'M' -> return Major

timeP :: Parser Time
timeP = do
    symbol "#time"
    -- verify at parse time that the rational has a denominator that is a power
    -- of two
    numerator <- L.decimal
    char '/'
    denominator <- L.decimal
    if isNotPowerofTwo denominator then error "Denominator not power of two" else
        return $ Time (numerator % denominator)
    where
        isNotPowerofTwo n = logBase 2.0 (fromIntegral denominator)

    


tempoP = symbol "#tempo"


-- Parse a pitchclass
pitchClassP :: Parser PitchClass
pitchClassP = do
    baseNote <- baseNoteP
    accidental <- accidentalP
    return $ PitchClass baseNote accidental


-- baseNoteP and accidentalP use arrays and maps, respectively as lookup tables
-- we should just pick one and stick with it
baseNoteP :: Parser BaseNote
baseNoteP = do
    noteName <- satisfy (\c -> c `elem` ['a'..'g']) 
    let noteMap = listArray ('a', 'g') [A .. G]
    return $ noteMap ! noteName
    

accidentalP :: Parser Accidental
accidentalP = do
    let accidentalStrs = ["ff", "tqf", "f", "qf", "qs", "s", "tqs", "ss"]
    accidentalName <- 
        -- there has to be a way to do something like this fold, but 
        -- for now, we can be satisfied with this amount of redundant typing
        -- foldr ((<|>)) $ map string accidentalStrs
        string "ff"  <|> string "tqf" <|>
        string "f"   <|> string "qf"  <|>
        string "qs"  <|> string "s"   <|>
        string "tqs" <|> string "ss"
    let accidentalMap = zip accidentalStrs [DoubleFlat .. DoubleSharp]
        -- the accidentalName parser would fail before this partial match would
        Just accidental = lookup accidentalName accidentalMap
    return accidental

