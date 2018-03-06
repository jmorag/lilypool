{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad.Identity
import qualified Control.Monad.State as S
import           Data.Void
import           Data.Typeable (Typeable)
import           Data.Either (rights)
import           Data.Data (Data)
import           Data.Maybe (fromMaybe)
import           Data.Ratio ((%))
import qualified Data.Bits as Bits
import qualified Data.Set as Set
import           Prelude hiding (String)

import Lilyval

-- Construct MusicState to keep track of rhythm, tempo, octave, key, grace
-- status, and slurs
-- Grace note status is something that we have in order to verify correct
-- number of beats in a bar. How we use it to eventually apply time stamps to
-- everything is TBD
data MusicState = MusicState { getRhythm    :: Dur
                             , getTempo     :: Tempo
                             , getOctave    :: Octave
                             , getKey       :: Key
                             , getTime      :: Time
                             , getString    :: String
                             , getGrace     :: Bool
                             , getBowChange :: Bool
                             } deriving (Show)

-- Sensible default state
defaultState = MusicState { getRhythm    = (1 % 4)
                          , getTempo     = Tempo (4 % 4) 60 --q=60
                          , getOctave    = 4 -- C4 is middle C
                          , getKey       = Key (PitchClass C Natural) Major
                          , getTime      = Time 4 4
                          , getString    = II
                          , getGrace     = False -- not a grace note
                          , getBowChange = True -- no slur
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

updateString :: String -> MState ()
updateString newString = do
    currState <- S.get
    S.put $ currState { getString = newString }

updateGrace :: MState ()
updateGrace = do
    currState <- S.get
    S.put $ currState { getGrace = not (getGrace currState) }

updateBow :: MState ()
updateBow = do
    currState <- S.get
    S.put $ currState { getBowChange = not (getBowChange currState) }
    

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

-- Parse the reserved time, key, and tempo symbols
keyP :: Parser ()
keyP = do 
    symbol "#key"
    pitchClass <- pitchClassP
    quality <- qualityP
    spaceConsumer >> eol
    let result = Key pitchClass quality
    S.lift $ updateKey result
    return ()
    where 
        qualityP = (char 'm' >> return Minor) <|> (char 'M' >> return Major)

timeP :: Parser ()
timeP = do
    symbol "#time"
    numerator <- numP
    char '/'
    -- We only accept powers of two as denominators
    denominator <- powerOfTwoP
    spaceConsumer >> eol
    let result = Time numerator denominator
    S.lift $ updateTime result
    return ()

tempoP :: Parser ()
tempoP = do 
    symbol "#tempo"
    noteLength <- rhythmP
    spaceConsumer
    symbol "="
    bpm <- numP
    spaceConsumer >> eol
    let result = Tempo noteLength bpm
    S.lift $ updateTempo result
    return ()

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
    
restP :: Parser Rest
restP = do
    char 'r'
    dur <- rhythmP
    tempo <- S.lift S.get >>= return . getTempo
    spaceConsumer
    return $ Rest (Length dur tempo)

noteP :: Parser Note
noteP = do
    pc <- pitchClassP
    dur <- rhythmP
    oct <- octaveP
    art <- articulationP
    tempo <- S.lift S.get >>= return . getTempo
    spaceConsumer
    return $ Note (Pitch pc oct) (Length dur tempo) art Nothing

unitP :: Parser MusicUnit
unitP =
    (try restP >>= return . R) <|> (try noteP >>= return . N) <|>
    -- parse chords
    do
        notes <- between (symbol "[") (symbol "]") (some noteP)
        case notes of
            [_] -> errorHelper "Don't write singleton chords"
            [n1, n2] -> return $ DStop n1 n2 
            [n1, n2, n3] -> return $ TStop n1 n2 n3
            [n1, n2, n3, n4] -> return $ QStop n1 n2 n3 n4
            _ -> errorHelper "Too many notes in chord"


stringP :: Parser String
stringP = do
    stringNum <- optional $ 
        (string "1" >> return I)   <|>
        (string "2" >> return II)  <|>
        (string "3" >> return III) <|>
        (string "4" >> return IV)
    case stringNum of
        Nothing -> spaceConsumer >> S.lift S.get >>= return . getString
        Just s -> do
            S.lift $ updateString s
            spaceConsumer
            return s

fingerP :: Parser Finger
fingerP = (string "0" >> return Open)  <|>
          (string "1" >> return One)   <|>
          (string "2" >> return Two)   <|>
          (string "3" >> return Three) <|>
          (string "4" >> return Four)

harmonicP :: Parser Harmonic
harmonicP = do
    isHarmonic <- optional (char 'o')
    case isHarmonic of
        Just 'o' -> return True
        Nothing  -> return False

fingeringP :: Parser (Maybe Fingering)
fingeringP = do
    notGiven <- optional (char '_')
    case notGiven of
        Just '_' -> spaceConsumer >> return Nothing
        Nothing -> do
            finger <- fingerP
            harmonic <- harmonicP
            string <- stringP
            return $ Just $ Fingering finger harmonic string

    
-- basic parser for a single measure
-- no slurs, chords, polyphony, or grace notes yet
measureP :: Parser Music
measureP = do
    notes <- some unitP
    eol
    fingerings <- some fingeringP
    some eol >> spaceConsumer
    case fingerings of
        [Nothing] -> do
            let measure = notes
            Time num den <- S.lift S.get >>= return . getTime
            if verifyMeasure measure (num % den) then
                return $ measure
            else errorHelper 
                "The value of the notes in \
                \ this measure do not match the time signature"

        fs -> let count [] = 0
                  count (n:ns) = case n of
                      N _ -> 1 + count ns
                      DStop _ _ -> 2 + count ns
                      TStop _ _ _ -> 3 + count ns
                      QStop _ _ _ _  -> 4 + count ns
              in 
                 if length fs /= count notes then 
                 errorHelper "Unequal numbers of fingers and notes given" else 

                 do 
                    let go note fingering = case note of 
                           N n -> N $ n { finger = fingering }
                           _ -> note
                    let measure = zipWith go notes fs

                    Time num den <- S.lift S.get >>= return . getTime
                    if verifyMeasure measure (num % den) then
                       return $ measure
                    else errorHelper 
                       "The value of the notes in \
                        \ this measure do not match the time signature"


-- this will need significant refactoring later to include errors with line
-- numbers and specific problems. Also, in the cases of chords and polyphony,
-- it currently assumes that every note in the chord is the same length and
-- that every voice in the polyphony has the same duration, which could
-- certainly not be the case
verifyMeasure :: Music -> Dur -> Bool
verifyMeasure music nBeats = accum music == nBeats
    where
    accum [] = 0 % 1
    accum (m:ms) = case m of
        N note           -> (dur $ nlen note) + accum ms
        DStop note _     -> (dur $ nlen note) + accum ms
        TStop note _ _   -> (dur $ nlen note) + accum ms
        QStop note _ _ _ -> (dur $ nlen note) + accum ms
        R rest           -> (dur $ rlen rest) + accum ms

-- This parses one line of music
lineP :: Parser (Either () Music)
lineP = (try keyP   >>= return . Left) <|>
        (try tempoP >>= return . Left) <|>
        (try timeP  >>= return . Left) <|>
        (measureP   >>= return . Right)

-- Null musical unit: returning this is a hack, and I don't want to do it,
-- but we'll keep it in case it becomes necessary for something else
musZero :: MusicUnit
musZero = R . Rest $ Length 0 (Tempo (4%4) 60)
            
-- This should parse a whole file
fileP :: Parser Music
fileP = do  
    spaceConsumer >> eol
    lines <- some (lexeme lineP)
    space >> eof
    return . concat $ rights lines
