module TeamEquity.TeamAffinity (

        -- Affinity and Equity
        generateTeamAffMap,
        getPairAff,
        getTeamEquity,

        -- Pretty Printer
        teamAffMapPP,
)where

-- LTS Imports
import qualified Data.Map as Map
import qualified Data.Map.Internal.Debug as DMap
import Numeric.Extra

-- User Imports
import TeamEquity.Types

innerMap :: [Developer] -> DevAff
innerMap = Map.fromListWith (\new_aff old_aff -> new_aff + old_aff) . Prelude.map (\dev -> (dev, 1))

generateTeamAffMap :: RepoInteractions -> TeamAff
generateTeamAffMap = Map.fromListWith (\new_map old_map -> Map.unionWith (\aff1 aff2 -> aff1 + aff2) new_map old_map) .
        Prelude.map (\pr -> (author pr, innerMap (reviewers pr)))

teamAffMapPP :: TeamAff -> IO ()
teamAffMapPP t = putStrLn $ DMap.showTreeWith (\k x -> show (k, Map.toAscList x)) False True t

getPairAff :: TeamAff -> Developer -> Developer -> Int
getPairAff map d1 d2 = (map Map.! d1) Map.! d2

stdDev :: [Float] -> Float
stdDev xs = sqrt . average . map ((^2) . (-) axs) $ xs
            where average = (/) <$> sum <*> realToFrac . length
                  axs = average xs

getTeamEquity :: TeamAff -> Float
getTeamEquity t = let values = Prelude.map intToFloat (concat (Prelude.map Map.elems (Map.elems t))) in
    stdDev values
