{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module LilyVal (
    PitchClass(..),
    Articulation(..)
) where


import           Data.Typeable (typeable)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Control.Exception
import           Control.Monad.Reader


data PitchClass = C | Dff | Btqs | Cqs | Dtqf | Bss | Cs | Df | Ctqs 
                | Dqf | Css | D | Eff | Dqs | Etqf | Ds | Ef | Fff 
                | Dtqs | Eqf | Ftqf | Dss | E | Ff | Eqs | Fqf | Es 
                | F | Gff | Etqs | Fqs | Gtqf | Ess | Fs | Gf | Ftqs 
                | Gqf | Fss | G | Aff | Gqs | Atqf | Gs | Af | Gtqs | Aqf 
                | Gss | A | Bff | Aqs | Btqf | As | Bf | Cff | Atqs | Bqf 
                | Ctqf | Ass | B | Cf | Bqs | Cqf | Bs

data Articulation = (!) | (-) | (.) | (>) | (+) | (^) 

type Dur = Rational
type Octave = Int

-- E | A | D | G for violin, but general for all strings
data String = One | Two | Three | Four 
type Harmonic = Bool
data Finger = Open | One | Two | Three | Four

data Tempo = Tempo Dur Int
data Fingering = Fingering Finger Harmonic String

data Note = Note { pitch  :: PitchClass
                 , dur    :: Dur
                 , oct    :: Octave
                 , art    :: Maybe Articulation
                 , tempo  :: Tempo
                 , finger :: Fingering
                 }

-- TODO chords and slurs and polyphony
