{-# LANGUAGE DeriveGeneric #-}

module Types where

-- LTS Imports
import qualified Data.HashMap.Strict as Map
import Data.Time.Calendar
import GHC.Generics


type Developer = String
type Affinity = Int
type RepoOwner = String
type RepoId = String
type User = String
type Password = String

type DevAff = Map.HashMap Developer Affinity

type TeamAff = Map.HashMap Developer DevAff

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