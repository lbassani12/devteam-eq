module TeamAffinity where

-- LTS Imports
import qualified Data.HashMap as Map

-- User Imports
import Types


innerMap :: [Developer] -> DevAff
innerMap = Map.fromListWith (\new_aff old_aff -> new_aff + old_aff) . Prelude.map (\dev -> (dev, 1))

generateTeamAffMap :: RepoInteractions -> TeamAff
generateTeamAffMap = Map.fromListWith (\new_map old_map -> Map.unionWith (\aff1 aff2 -> aff1 + aff2) new_map old_map) .
        Prelude.map (\pr -> (author pr, innerMap (reviewers pr)))