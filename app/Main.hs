{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import TeamAffinity
import Data.Time.Calendar
import Github

main :: IO ()
main = do
    let
        credentials = setCredentials "lbassani12" "e9bf8f472a257568c3932fe052980e971dae6db5"
        repo = setRepo "Microsoft" "TypeScript"
        date_range = setDate (fromGregorian 2019 05 01) (fromGregorian 2019 10 01)
    res <- getIssues repo date_range credentials
    print (res, length res)
