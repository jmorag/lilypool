{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module LilyVal (
) where


import           Data.Typeable (typeable)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Control.Exception
import           Control.Monad.Reader


data Pitchclass = C | Dff | Btqs | Cqs | Dtqf | Bss | Cs | Df | Ctqs 
                | Dqf | Css | D | Eff | Dqs | Etqf | Ds | Ef | Fff 
                | Dtqs | Eqf | Ftqf | Dss | E | Ff | Eqs | Fqf | Es 
                | F | Gff | Etqs | Fqs | Gtqf | Ess | Fs | Gf | Ftqs 
                | Gqf | Fss | G | Aff | Gqs | Atqf | Gs | Af | Gtqs | Aqf 
                | Gss | A | Bff | Aqs | Btqf | As | Bf | Cff | Atqs | Bqf 
                | Ctqf | Ass | B | Cf | Bqs | Cqf | Bs

