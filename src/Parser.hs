{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Control.Monad.State as S
import           Data.Void
import           Data.Typeable (Typeable)
import           Data.Data (Data)
import           Data.Maybe (fromMaybe)
import           Data.Ratio ((%))
import qualified Data.Bits as Bits
import qualified Data.Set as Set
import           Prelude hiding (String)

import Lilyval

-- Construct MusicState to keep track of rhythm, tempo, octave, key, etc.
data MusicState = MusicState { getRhythm :: Dur
                             , getTempo  :: Tempo
                             , getOctave :: Octave
                             , getKey    :: Key
                             , getTime   :: Time
                             }
-- Sensible default state
defaultState = MusicState { getRhythm = (4 % 4)
                          , getTempo  = Tempo (4 % 4) 60 --q=60
                          , getOctave = 4 -- C4 is middle C
                          , getKey    = Key (PitchClass C Natural) Major
                          , getTime   = Time 4 4
                          }

type MState = S.State MusicState

updateRhythm :: Dur -> MState ()
updateRhythm newRhythm = do
    currState <- S.get
    S.put $ currState { getRhythm = newRhythm }

updateTempo :: Tempo -> MState ()
updateTempo newTempo = do
    currState <- S.get
    S.put $ currState { getTempo = newTempo }

updateOctave :: Octave -> MState ()
updateOctave newOctave = do
    currState <- S.get
    S.put $ currState { getOctave = newOctave }

updateKey :: Key -> MState ()
updateKey newKey = do
    currState <- S.get
    S.put $ currState { getKey = newKey }

updateTime :: Time -> MState ()
updateTime newTime = do
    currState <- S.get
    S.put $ currState { getTime = newTime }

type Parser = ParsecT CustomError Text MState

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
errorHelper = fancyFailure . Set.singleton . ErrorCustom . CustomError

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

---------------------------------------------------------------------
{- Begin music parsing -}
---------------------------------------------------------------------
{- TODO: refactor the following parsers to update MState
 - TODO: decide if they should actually return things or not...
 - The following parsers should deterministically return Parser ~music_thing~
 - rhythmP
 - tempoP
 - octaveP
 - keyP
 - timeP
 - To enforce this behavior, the parsers will look for their respective
 - characters, and should they find them, return that structure and update the
 - map. Otherwise, they simply query the map for that state and return it,
 - leaving the map unchanged
 -}

-- Parse the reserved time, key, and tempo symbols
keyP :: Parser Key
keyP = do 
    symbol "#key"
    pitchClass <- pitchClassP
    quality <- qualityP
    spaceConsumer >> eol
    let result = Key pitchClass quality
    S.lift $ updateKey result
    return result 
    where 
        -- qualityP :: Parser Quality
        qualityP = (char 'm' >> return Minor) <|> (char 'M' >> return Major)

timeP :: Parser Time -- possibly Parser ()
timeP = do
    symbol "#time"
    numerator <- numP
    char '/'
    -- We only accept powers of two as denominators
    denominator <- powerOfTwoP
    spaceConsumer >> eol
    let result = Time numerator denominator
    S.lift $ updateTime result
    return result 

tempoP :: Parser Tempo -- possibly Parser ()
tempoP = do 
    symbol "#tempo"
    noteLength <- rhythmP
    spaceConsumer
    symbol "="
    bpm <- numP
    spaceConsumer >> eol
    let result = Tempo noteLength bpm
    S.lift $ updateTempo result
    return result 

-- This is where it gets more interesting
-- rhythmP looks like a textbook example of why the Maybe monad exists...
-- this is a good sign that we should use that
rhythmP :: Parser Dur
rhythmP = do
    baseLength <- optional powerOfTwoP
    case baseLength of
        Nothing -> S.lift S.get >>= return . getRhythm
        Just bl -> do
            isDotted <- optional (char ',')
            case isDotted of
                Nothing -> do
                    let result = (1 % bl)
                    S.lift $ updateRhythm result
                    return result
                Just ',' -> do
                    let result = (3 % (bl * 2))
                    S.lift $ updateRhythm result
                    return result

-- Not as bad a design pattern as rhythmP, but still not ideal
octaveP :: Parser Octave
octaveP = do
    indicator <- optional (char '_')
    case indicator of
        Nothing -> S.lift S.get >>= return . getOctave
        Just '_' -> do
            oct <- numP
            S.lift $ updateOctave oct
            return oct

pitchClassP :: Parser PitchClass
pitchClassP = do
    baseNote <- baseNoteP
    accidental <- accidentalP
    return $ PitchClass baseNote accidental

baseNoteP :: Parser BaseNote
baseNoteP =
    (string "a" >> return A) <|> (string "b" >> return B) <|>
    (string "c" >> return C) <|> (string "d" >> return D) <|>
    (string "e" >> return E) <|> (string "f" >> return F) <|>
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


-- We only support one articulation per note
articulationP :: Parser Articulation
articulationP = 
    (string "!"  >> return Staccatissimo) <|>
    (string "."  >> return Staccato)      <|>
    (string "^"  >> return VAccent)       <|>
    (string ">"  >> return Accent)        <|>
    (string "-." >> return Portato)       <|>
    (string "-"  >> return Tenuto)        <|>
    (string "+"  >> return LHPizz)        <|>
    -- None also goes at the end because it always succeeds
    (string ""   >> return None)
    
restP :: Parser Primitive
restP = do
    char 'r'
    length <- rhythmP
    tempo <- S.lift S.get >>= return . getTempo
    return $ Rest length tempo False -- deal with grace notes later

noteP :: Parser Primitive
noteP = do
    pc <- pitchClassP
    length <- rhythmP
    oct <- octaveP
    art <- articulationP
    tempo <- S.lift S.get >>= return . getTempo
    -- fingerings and grace notes not implemented for now
    return $ Note pc length oct art tempo Nothing False

primP :: Parser Primitive
primP = try restP <|> noteP 
