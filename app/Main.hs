{-# LANGUAGE OverloadedStrings #-}

module Main where

-- LTS Imports
import Data.Time.Calendar

-- User Imports
import TeamEquity

main :: IO ()
main = do
    let
        credentials = setCredentials <Your User> <Your Password>
        repo = setRepo "Microsoft" "TypeScript"
        date_range = setDate (fromGregorian 2019 10 01) (fromGregorian 2019 10 21)
    pr1 <- getRepoInteractions repo date_range credentials
    let maps = generateTeamAffMap pr1
        eq = getTeamEquity maps
        pair_aff = getPairAff maps "sandersn" "rbuckton"
    teamAffMapPP maps
    print (eq, pair_aff)
