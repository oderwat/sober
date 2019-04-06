module Brewer.Internal where

import           Data.Char       (isSpace)
import           Data.List       (dropWhile, dropWhileEnd, isPrefixOf)
import           Data.List.Split (splitOn)

trim = dropWhileEnd isSpace . dropWhile isSpace

splitColon :: String -> [String]
splitColon = splitOn ": "

findAndSplit :: String -> [String] -> [String]
findAndSplit find entries
  | null entries = []
  | null found = []
  | otherwise = map trim $ splitOn ", " $ drop (length find) (head found)
  where
    found = filter (isPrefixOf find) entries
