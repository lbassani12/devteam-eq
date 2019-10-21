{-# LANGUAGE OverloadedStrings #-}

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

-- User Imports
import Types
import Parser

setCredentials u p = basicAuth u p

setDate :: Day -> Day -> DateRange
setDate from to = Range from to

setRepo :: RepoOwner -> RepoId -> Repository
setRepo r_owner r_id = Repo { r_owner = r_owner
                            , r_id = r_id
                            , pullRequests = []
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
            let (Just some_header) = responseHeader resp "Link"
                (issues, err) = case parseIssues (responseBody resp :: Value) of
                    Right i -> (i, "todo bien")
                    Left err -> ([], err)
                -- (Just issues) = parseIssues (responseBody resp :: Value)
            -- trace (show (issues, err)) (return ())
            if page == last_page
            then return issues
            else do
                let (Right (next_page, q_last)) = parseHeader $ BS.unpack some_header
                asd <- liftIO $ getIssues' repo (Range from to) auth (next_page, q_last)
                return $ issues ++ asd
    return a

getPullRequest issue repo auth = do
    a <- runReq defaultHttpConfig $ do
        let headers = header "User-Agent" "lbassani12"
            author = i_author issue
            pullreq = DText.pack $ show (number issue)
            repo_owner = DText.pack $ r_owner repo
            repo_id = DText.pack $ r_id repo
        resp <- req GET
            (https "api.github.com" /: "repos" /: repo_owner /: repo_id /: "pulls" /: pullreq)
            NoReqBody
            jsonResponse
            (headers <> auth)
        let (Just reviews) = parseReviewers (responseBody resp :: Value)
        return reviews
    return a
