{-# LANGUAGE OverloadedStrings #-}

module Brewer.Internal where

import           Data.Char       (isSpace)
import           Data.List       (dropWhile, dropWhileEnd, isPrefixOf, elemIndex)
import           Data.List.Split (splitOn)
import           Data.Text       (Text (..))
import qualified Data.Text       as T

splitAtFirstSpace :: Text -> Maybe (Text, Text)
splitAtFirstSpace str =
  case T.findIndex isSpace cleaned of
    Nothing -> Nothing
    Just i -> Just (T.strip $ T.take i cleaned, T.strip $ T.drop (i + 1) cleaned)
  where
    cleaned = T.dropWhile isSpace str

splitColon :: Text -> [Text]
splitColon = T.splitOn ": "

findAndSplit :: Text -> [Text] -> [Text]
findAndSplit find entries
  | null entries = []
  | null found = []
  | otherwise = map T.strip $ T.splitOn ", " $ T.drop (T.length find) (head found)
  where
    found :: [Text]
    found = filter (T.isPrefixOf find) entries
