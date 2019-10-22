{-# LANGUAGE OverloadedStrings #-}

module Main where

-- LTS Imports
import Data.Time.Calendar

-- User Imports
import TeamAffinity
import Github

main :: IO ()
main = do
    let
        credentials = setCredentials "lbassani12" "e9bf8f472a257568c3932fe052980e971dae6db5"
        repo = setRepo "Microsoft" "TypeScript"
        date_range = setDate (fromGregorian 2019 10 01) (fromGregorian 2019 10 21)
    pr1 <- getRepoInteractions repo date_range credentials
    let maps = generateTeamAffMap pr1
    print (length pr1, pr1, maps)
