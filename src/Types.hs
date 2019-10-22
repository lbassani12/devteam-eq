{-# LANGUAGE DeriveGeneric #-}

module Types where

-- LTS Imports
import qualified Data.HashMap as Map
import Data.Time.Calendar
import GHC.Generics
import Control.Exception
import Text.Parsec (ParseError)


type Developer = String
type Affinity = Int
type RepoOwner = String
type RepoId = String
type User = String
type Password = String

data GitException
    = ParseHeaderError ParseError
    | ParseIssuesError String
    | ParseRevError String
    deriving (Show)

instance Exception GitException

type DevAff = Map.HashMap Developer Affinity

type TeamAff = Map.HashMap Developer DevAff

data Issue = Issue { i_author :: Developer
                   , number :: Int
} deriving(Show, Generic)

type Issues = [Issue]

data PullRequest = PR { author :: Developer
                      , reviewers :: [Developer]
                      } deriving (Eq, Show)

type RepoInteractions = [PullRequest]

data Repository = Repo { r_owner :: RepoOwner
                        , r_id :: RepoId
                        } deriving (Eq, Show)

data DateRange = Range Day Day