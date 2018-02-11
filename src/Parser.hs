{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad (void)
import           Data.Void
import           Data.Typeable (Typeable)
import           Data.Data (Data)
import           Data.Maybe (fromMaybe)
import           Data.Ratio ((%))
import qualified Data.Bits as Bits
import qualified Data.Set as S

import Lilyval

type Parser = Parsec CustomError Text

{- 
 - Many examples taken from 
 - https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
 -}
-- Define custom error type
data CustomError = CustomError Text
    deriving (Eq, Data, Typeable, Ord, Read, Show)

instance ShowErrorComponent CustomError where
    showErrorComponent (CustomError msg) = 
        "error: " ++ T.unpack msg

errorHelper :: Text -> Parser a
errorHelper = fancyFailure . S.singleton . ErrorCustom . CustomError

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

numP :: Parser Int
numP = do
    num <- L.decimal
    if num < 0 then errorHelper "Expecting natural number only"
    else return num

-- Often, we need to parse a number, but make sure that it is a power of two
powerOfTwoP :: Parser Int
powerOfTwoP = do
    num <- numP
    if not $ isPowerofTwo num then errorHelper "Expecting a power of two"
    else return num
    where
        isPowerofTwo n = Bits.countLeadingZeros n + Bits.countTrailingZeros n 
                         == Bits.finiteBitSize n - 1

{- Begin music parsing -}
-- Parse the reserved time, key, and tempo symbols
keyP :: Parser Key
keyP = do 
    symbol "#key"
    pitchClass <- pitchClassP
    quality <- qualityP
    spaceConsumer >> eol
    return $ Key pitchClass quality

qualityP :: Parser Quality
qualityP = (char 'm' >> return Minor) <|> (char 'M' >> return Major)

timeP :: Parser Time
timeP = do
    symbol "#time"
    numerator <- numP
    char '/'
    -- We only accept powers of two as denominators
    denominator <- powerOfTwoP
    eol
    return $ Time numerator denominator

tempoP :: Parser Tempo
tempoP = do 
    symbol "#tempo"
    noteLength <- rhythmP
    spaceConsumer
    symbol "="
    bpm <- numP
    spaceConsumer >> eol
    return $ Tempo noteLength bpm

rhythmP :: Parser Dur
rhythmP = do
    baseLength <- powerOfTwoP
    isDotted <- optional (char ',')
    return $ case isDotted of
        Nothing -> (1 % baseLength)
        Just ',' -> (3 % (baseLength * 2))

pitchClassP :: Parser PitchClass
pitchClassP = do
    baseNote <- baseNoteP
    accidental <- accidentalP
    return $ PitchClass baseNote accidental

baseNoteP :: Parser BaseNote
baseNoteP =
    (string "a" >> return A) <|>
    (string "b" >> return B) <|>
    (string "c" >> return C) <|>
    (string "d" >> return D) <|>
    (string "e" >> return E) <|>
    (string "f" >> return F) <|>
    (string "g" >> return G)

accidentalP :: Parser Accidental
accidentalP =
    (string "ff"  >> return DoubleFlat)  <|>
    (string "tqf" >> return TQFlat)      <|>
    (string "f"   >> return Flat)        <|>
    (string "qf"  >> return QFlat)       <|>
    (string "qs"  >> return QSharp)      <|>
    (string "s"   >> return Sharp)       <|>
    (string "tqs" >> return TQSharp)     <|>
    (string "ss"  >> return DoubleSharp) <|>
    (string ""    >> return Natural) 
    -- natural has to go at the end because it always succeeds


octaveP :: Parser Octave
octaveP = char '_' >> numP >>= return

articulationP :: Parser Articulation
articulationP = 
    (string "!"  >> return Staccatissimo) <|>
    (string "."  >> return Staccato)      <|>
    (string "^"  >> return VAccent)       <|>
    (string ">"  >> return Accent)        <|>
    (string "-." >> return Portato)       <|>
    (string "-"  >> return Tenuto)        <|>
    (string "+"  >> return LHPizz)        <|>
    (string ""   >> return None)
    -- None also goes at the end because it always succeeds
    
