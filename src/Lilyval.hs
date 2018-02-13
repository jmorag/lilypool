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

data BaseNote = A | B | C | D | E | F | G 
    deriving (Enum, Show)

data Accidental = DoubleFlat | TQFlat | Flat | QFlat | Natural
                | QSharp | Sharp | TQSharp | DoubleSharp
    deriving (Enum, Show)

data PitchClass = PitchClass BaseNote Accidental
    deriving Show

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
    deriving Show

data Primitive = 
    Note { pitch  :: PitchClass
         , dur    :: Dur
         , oct    :: Octave
         , art    :: Articulation
         , tempo  :: Tempo
         , finger :: Maybe Fingering
         , grace  :: Bool
         } | 
    Rest { dur :: Dur
         , tempo :: Tempo
         , grace  :: Bool
         } -- it is possible that the compiler will complain 
           -- about records with the same names
    deriving Show

data Music = Unit Primitive  |
             Music :+: Music | -- notes under a slur
             Music :=: Music | -- notes in a chord
             Passage [Music] | -- a passage 
             Polyphony [[Music]] -- arbitrary polyphony
    deriving Show
-- (possibly consider using Sequence instead of List for passage)


-- Speical annotations
data Quality = Major | Minor
    deriving Show
data Key     = Key PitchClass Quality
    deriving Show
-- Time signatures can't be ratios since then they'll be reduced
data Time    = Time Int Int
    deriving Show
data Tempo   = Tempo Dur Int
    deriving Show

