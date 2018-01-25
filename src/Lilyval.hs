{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Lilyval where


import           Data.Typeable (Typeable)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Control.Exception
import           Control.Monad.Reader
import           Prelude hiding (String)


data PitchClass = C | Dff | Btqs | Cqs | Dtqf | Bss | Cs | Df | Ctqs 
                | Dqf | Css | D | Eff | Dqs | Etqf | Ds | Ef | Fff 
                | Dtqs | Eqf | Ftqf | Dss | E | Ff | Eqs | Fqf | Es 
                | F | Gff | Etqs | Fqs | Gtqf | Ess | Fs | Gf | Ftqs 
                | Gqf | Fss | G | Aff | Gqs | Atqf | Gs | Af | Gtqs | Aqf 
                | Gss | A | Bff | Aqs | Btqf | As | Bf | Cff | Atqs | Bqf 
                | Ctqf | Ass | B | Cf | Bqs | Cqf | Bs

-- ! . ^ > - -. +
data Articulation = Staccatissimo | Staccato | VAccent | Accent 
                    | Tenuto | Portato | LHPizz | None

type Dur = Rational
type Octave = Int

-- E | A | D | G for violin, but general for all strings
data String = I | II | III | IV 
data Finger = Open | One | Two | Three | Four
type Harmonic = Bool

data Tempo = Tempo Dur Int
data Fingering = Fingering Finger Harmonic String

data Primitive = Note { pitch  :: PitchClass
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
                 }

data Music = Prim Primitive  |
             Music :+: Music | -- notes under a slur
             Music :=: Music | -- notes in a chord
             Passage [Music] | -- a passage 
             Polyphony [[Music]] -- arbitrary polyphony
-- (possibly consider using Sequence instead of List for passage)

-- Speical annotations
data Quality = Major | Minor
data Key     = Key PitchClass Quality
type Time    = Rational

