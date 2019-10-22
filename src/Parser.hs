{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

-- LTS Imports
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Text.Parsec
import Text.Parsec.String

-- User Imports
import Types

-- Parsers for JSON Responses
parseSingleItem :: Value -> Aeson.Parser Issue
parseSingleItem = withObject "single_item" $ \item -> do
    number <- item .: "number"
    user <- item .: "user"
    i_author <- user .: "login"
    return Issue {..}

parseItems :: Value -> Aeson.Parser Issues
parseItems = withObject "array of items" $ \o -> do
    items <- o .: "items"
    mapM parseSingleItem items

parseSingleReviewer :: Value -> Aeson.Parser String
parseSingleReviewer = withObject "reviewer" $ \o -> do
    reviewer <- o .: "login"
    return reviewer

parseReqReviewers :: Value -> Aeson.Parser [String]
parseReqReviewers = withObject "array of reviewers" $ \o -> do
    reviews <- o .: "requested_reviewers"
    mapM parseSingleReviewer reviews

parseReviewers value = Aeson.parseEither parseReqReviewers value
parseIssues value = Aeson.parseEither parseItems value

-- Parsers for Header Responses
parsePage :: String -> Parser (String, String)
parsePage s = do
    manyTill anyChar $ try (string "page")
    char '='
    x <- many1 digit
    manyTill anyChar $ try (string "rel")
    char '='
    rel <- between (char '"') (char '"') (string s)
    return (x, rel)

parseNextPage = parsePage "next"

parseLastPage = parsePage "last"

parsePrevPage = parsePage "prev"

parseNextAndLastPage :: Parser (String, String)
parseNextAndLastPage = do
    (page1, label1) <- (try parseNextPage) <|> parsePrevPage
    (page2, label2) <- (try parseLastPage) <|> parseNextPage
    if label1 == "prev"
    then do
        (page3, label3) <- parseLastPage
        return (page2, page3)
    else return (page1, page2)

myParse :: Parser a -> String -> Either ParseError a
myParse rule text = parse rule "Error in" text

parseHeader header = myParse parseNextAndLastPage header