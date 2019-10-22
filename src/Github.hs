{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Github where

-- LTS Imports
import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import Data.Text as DText
import System.Environment
import Control.Monad.Except
import System.IO.Error
import Data.Time.Calendar
import Data.ByteString.Char8 as BS
import Debug.Trace
import Control.Exception
import Control.Monad.HT as HT

-- User Imports
import Types
import Parser

setCredentials u p = basicAuth u p

setDate :: Day -> Day -> DateRange
setDate from to = Range from to

setRepo :: RepoOwner -> RepoId -> Repository
setRepo r_owner r_id = Repo { r_owner = r_owner
                            , r_id = r_id
                            }

getIssues repo range auth = getIssues' repo range auth ("1", "0")

getIssues' repo (Range from to) auth (page, last_page) = do
    a <- runReq defaultHttpConfig $ do
            let headers = header "User-Agent" "lbassani12"
                repo_owner = r_owner repo
                repo_id = r_id repo
                date_from = showGregorian from
                date_to = showGregorian to
                q_repo = DText.pack $ "is:pr repo:" ++ repo_owner ++ "/" ++ repo_id ++ " -review:none created:" ++ date_from ++ ".." ++ date_to
                packed_page = DText.pack page
                q_parms = "q"       =: (q_repo) <>
                        "sort"      =: ("created" :: Text) <>
                        "direction" =: ("asc" :: Text) <>
                        "page"      =: (packed_page)
            resp <- req GET
                (https "api.github.com" /: "search" /: "issues")
                NoReqBody
                jsonResponse
                (q_parms <> headers <> auth)
            let ((next_page, q_last_page), issues) =
                        (
                        case responseHeader resp "Link" of
                            Just h -> if page == last_page
                                      then (last_page, last_page)
                                      else case parseHeader $ BS.unpack h of
                                            Right (n, l) -> (n, l)
                                            Left err -> ("1", "1")
                            Nothing -> ("1", "1")
                        ,
                        case parseIssues (responseBody resp :: Value) of
                            Right i -> i
                            Left err -> throw (ParseIssuesError err)
                        )
            if page == q_last_page
            then return issues
            else do
                iss <- liftIO $ getIssues' repo (Range from to) auth (next_page, q_last_page)
                return $ issues ++ iss
    return a

getPullRequest repo auth issue = do
    a <- runReq defaultHttpConfig $ do
        let headers = header "User-Agent" "lbassani12"
            author = i_author issue
            pull_req = DText.pack $ show (number issue)
            repo_owner = DText.pack $ r_owner repo
            repo_id = DText.pack $ r_id repo
        resp <- req GET
            (https "api.github.com" /: "repos" /: repo_owner /: repo_id /: "pulls" /: pull_req)
            NoReqBody
            jsonResponse
            (headers <> auth)
        let reviewers = case parseReviewers (responseBody resp :: Value) of
                Right r -> r
                Left err -> throw (ParseRevError err)
        return PR {..}
    return a

getRepoInteractions repo range auth = do
    issues <- getIssues repo range auth
    prs <- HT.map (getPullRequest repo auth) issues
    return prs
