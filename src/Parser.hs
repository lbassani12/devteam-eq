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

parse_single_item :: Value -> Aeson.Parser Issue
parse_single_item = withObject "single_item" $ \item -> do
    number <- item .: "number"
    user <- item .: "user"
    i_author <- user .: "login"
    return Issue {..}

parse_items :: Value -> Aeson.Parser [Issue]
parse_items = withObject "array of items" $ \o -> do
    items <- o .: "items"
    mapM parse_single_item items

parseIssues value = Aeson.parseMaybe parse_items value

parsePage :: Parser String
parsePage = do
    manyTill anyChar $ try (string "page")
    char '='
    x <- many1 digit
    return x

parseNextAndLastPage :: Parser (String, String)
parseNextAndLastPage = do
    next_page <- parsePage
    last_page <- parsePage
    return (next_page, last_page)

myparse :: Parser a -> String -> Either ParseError a
myparse rule text = parse rule "Error in" text

parseHeader header = myparse parseNextAndLastPage header