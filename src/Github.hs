{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Github where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Req
import Data.Text
import System.Environment
import Control.Monad.Except
import System.IO.Error
import Data.Time.Calendar
import GHC.Generics

import TeamAffinity

type RepoOwner = String
type RepoId = String
type User = String
type Password = String

data Issue = Issue { i_author :: Developer
                     , number :: Int
} deriving(Show, Generic)

data PullRequest = PR { author :: Developer
                      , reviewers :: [Developer]
                      } deriving (Eq, Show)

data Repository = Repo { r_owner :: RepoOwner
                       , r_id :: RepoId
                       , pullRequests :: [PullRequest]
                       } deriving (Eq, Show)

data DateRange = Range Day Day

parse_single_item :: Value -> Parser Issue
parse_single_item = withObject "single_item" $ \item -> do
    number <- item .: "number"
    user <- item .: "user"
    i_author <- user .: "login"
    return Issue {..}

parse_items :: Value -> Parser [Issue]
parse_items = withObject "array of items" $ \o -> do
    items <- o .: "items"
    mapM parse_single_item items

parseIssues value = parseMaybe parse_items value

setCredentials u p = basicAuth u p

setDate :: Day -> Day -> DateRange
setDate from to = Range from to

setRepo :: RepoOwner -> RepoId -> Repository
setRepo r_owner r_id = Repo { r_owner = r_owner
                            , r_id = r_id
                            , pullRequests = []
                            }

-- getIssues :: Repository -> DateRange -> IO ()
getIssues repo (Range from to) auth =
    runReq defaultHttpConfig $ do
    let headers = header "User-Agent" "lbassani12"
        repo_owner = r_owner repo
        repo_id = r_id repo
        date_from = showGregorian from
        date_to = showGregorian to
        q_repo = pack $ "is:pr repo:" ++ repo_owner ++ "/" ++ repo_id ++ " -review:none created:" ++ date_from ++ ".." ++ date_to
        q_parms = "q"       =: ( q_repo :: Text) <>
                "sort"      =: ("created" :: Text) <>
                "direction" =: ("asc" :: Text)
    r <- req GET                                            -- method
        (https "api.github.com" /: "search" /: "issues")    -- safe by construction URL
        NoReqBody                                           -- use built-in options or add your own
        jsonResponse                                        -- specify how to interpret response
        (q_parms <> headers <> auth)                        -- query params, headers, explicit port number, etc.
    return $ parseIssues (responseBody r :: Value)

-- getPRs :: Maybe [Issue] -> IO ()
-- getPRs m = print m
