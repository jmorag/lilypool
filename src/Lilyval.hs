{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Lilyval where


import           Data.Typeable (Typeable)
import qualified Data.Text as T
-- Unsure why reader or exception need to be in this file...
import           Control.Exception
import           Control.Monad.Reader
import           Prelude hiding (String)
import           Data.Ratio (Ratio)

-- import           Prettyprint

data BaseNote = A | B | C | D | E | F | G 
    deriving (Enum, Show)

data Accidental = DoubleFlat | TQFlat | Flat | QFlat | Natural
                | QSharp | Sharp | TQSharp | DoubleSharp
    deriving (Enum, Show)

data PitchClass = PitchClass BaseNote Accidental

-- ! . ^ > - -. +
data Articulation = Staccatissimo | Staccato | VAccent | Accent 
                    | Tenuto | Portato | LHPizz | None
    deriving Show

type Dur = Ratio Int
type Octave = Int

-- E | A | D | G for violin, but general for all strings
data String = I | II | III | IV 
    deriving Show
data Finger = Open | One | Two | Three | Four
    deriving Show
type Harmonic = Bool

data Fingering = Fingering Finger Harmonic String

-- Speical annotations
data Quality = Major | Minor
    deriving Show
data Key     = Key PitchClass Quality

-- Time signatures can't be ratios since then they'll be reduced
data Time    = Time Int Int
    deriving Show
data Tempo   = Tempo Dur Int


data Pitch  = Pitch PitchClass Octave
data Length = Length { dur :: Dur, tempo :: Tempo }

data Rest = Rest { rlen :: Length }
data Note = Note { pitch  :: Pitch
                 , nlen   :: Length
                 , art    :: Articulation
                 , finger :: Maybe Fingering
                 }

data MusicUnit = N Note  |
                 DStop Note Note |
                 TStop Note Note Note |
                 QStop Note Note Note Note |
                 R Rest 
                 deriving Show

type Music = [MusicUnit]

-- Pretty printing

instance Show PitchClass  where
    show (PitchClass note accidental) = show note ++ " " ++ show accidental

instance Show Fingering where
    show (Fingering f False s) = show f ++ " " ++ show s
    show (Fingering f True s)  = show f ++ " o " ++ show s

instance Show Key where
    show (Key pc qual) = show pc ++ " " ++ show qual

instance Show Tempo where
    show (Tempo noteLen bpm) = show noteLen ++ "=" ++ show bpm

instance Show Pitch where
    show (Pitch pc oct) = show pc ++ "_" ++ show oct

instance Show Length where
    show l = show (dur l) ++ " at " ++ show (tempo l)

instance Show Rest where
    show r = "r" ++ show (rlen r)

instance Show Note where
    show n = show (pitch n) ++ " " ++ 
             show (nlen n) ++ " " ++
             show (art n) ++ " " ++
             show (finger n) ++ "\n"
    
