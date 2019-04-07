module Brewer.Internal where

import           Data.Char       (isSpace)
import           Data.List       (dropWhile, dropWhileEnd, isPrefixOf, elemIndex)
import           Data.List.Split (splitOn)

trim = dropWhileEnd isSpace . dropWhile isSpace

splitAtFirstSpace :: String -> Maybe (String, String)
splitAtFirstSpace str =
   case elemIndex ' ' cleaned of
    Nothing -> Nothing
    Just i -> Just (trim $ take i cleaned, trim $ drop (i+1) cleaned)
   where
    cleaned = dropWhile isSpace str

splitColon :: String -> [String]
splitColon = splitOn ": "

findAndSplit :: String -> [String] -> [String]
findAndSplit find entries
  | null entries = []
  | null found = []
  | otherwise = map trim $ splitOn ", " $ drop (length find) (head found)
  where
    found = filter (isPrefixOf find) entries
