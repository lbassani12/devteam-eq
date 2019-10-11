module TeamAffinity
    ( Developer
    ) where

import qualified Data.HashMap.Strict as Map


type Developer = String
type Affinity = Int

type DevAff = Map.HashMap Developer Affinity

type TeamAff = Map.HashMap Developer DevAff
