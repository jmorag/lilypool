{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)
import Data.Void
import           Data.Typeable (Typeable)
import           Data.Data (Data)

import           Data.Array (listArray, (!))
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
    spaceConsumer >> eol
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
    numerator <- L.decimal :: Parser Int
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
    bpm <- L.decimal :: Parser Int
    spaceConsumer >> eol
    return $ Tempo noteLength bpm

rhythmP :: Parser Dur
rhythmP = do
    baseLength <- powerOfTwoP
    isDotted <- observing $ string ","
    return $ case isDotted of
        Left _ -> (1 % baseLength)
        Right "," -> (3 % (baseLength * 2))



-- Often, we need to parse a number, but make sure that it is a power of two
numP :: Parser Int
numP = do
    num <- L.decimal
    if num < 0 then errorHelper "Expecting natural number only"
    else return num

powerOfTwoP :: Parser Int
powerOfTwoP = do
    num <- L.decimal
    if not $ isPowerofTwo num then errorHelper "Expecting a power of two"
    --ErrorCustom "Denominator not power of two" 
    else return num
    where
        isPowerofTwo n = Bits.countLeadingZeros n + Bits.countTrailingZeros n 
                         == Bits.finiteBitSize n - 1


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
    let accidentalStrs = ["ff", "tqf", "f", "qf", "", "qs", "s", "tqs", "ss"]
    accidentalName <- 
        -- there has to be a way to do something like this fold, but 
        -- for now, we can be satisfied with this amount of redundant typing
        -- foldr ((<|>)) $ map string accidentalStrs
        string "ff"  <|> string "tqf" <|>
        string "f"   <|> string "qf"  <|> 
        string "qs"  <|> string "s"   <|>
        string "tqs" <|> string "ss"  <|> string "" 
        -- natural has to go at the end because it always succeeds

    let accidentalMap = zip accidentalStrs [DoubleFlat .. DoubleSharp]
        -- the accidentalName parser would fail before this partial match would
        Just accidental = lookup accidentalName accidentalMap
    return accidental


data CustomError = CustomError Text
    deriving (Eq, Data, Typeable, Ord, Read, Show)

instance ShowErrorComponent CustomError where
    showErrorComponent (CustomError msg) = 
        "error: " ++ T.unpack msg

errorHelper :: Text -> Parser a
errorHelper = fancyFailure . S.singleton . ErrorCustom . CustomError
