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
                packedPage = DText.pack page
                q_parms = "q"       =: (q_repo) <>
                        "sort"      =: ("created" :: Text) <>
                        "direction" =: ("asc" :: Text) <>
                        "page"      =: (packedPage)
            resp <- req GET                                         -- method
                (https "api.github.com" /: "search" /: "issues")    -- safe by construction URL
                NoReqBody                                           -- use built-in options or add your own
                jsonResponse                                        -- specify how to interpret response
                (q_parms <> headers <> auth)                        -- query params, headers, explicit port number, etc.
            let (Just some_header) = responseHeader resp "Link"
                (Just issues) = parseIssues (responseBody resp :: Value)
            if page == last_page
            then return issues
            else do
                let (Right (next_page, q_last)) = parseHeader $ BS.unpack some_header
                asd <- liftIO $ getIssues' repo (Range from to) auth (next_page, q_last)
                return $ issues ++ asd
    return a
