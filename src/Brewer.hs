module Brewer
  ( getPackageList
  , packageInfo
  , doBrewCmd
  ) where

import           Control.Applicative
import           Data.Maybe
import           Network.URI     (URI, parseURI)
import           System.Exit     (ExitCode (ExitSuccess))
import           System.Process  (readProcessWithExitCode)

import           Brewer.Internal

brewBin = "/usr/local/bin/brew"

data Package = Package
  { name        :: String
  , version     :: String
  , description :: String
  , link        :: Maybe URI
  , build       :: [String]
  , required    :: [String]
  } deriving (Show)

doBrewCmd :: [String] -> IO [String]
doBrewCmd args = do
  let cmdin = ""
  (errCode, cmdout, cmderr) <- readProcessWithExitCode brewBin args cmdin
  return $
    case errCode of
      ExitSuccess -> lines cmdout
      _           -> []

getPackageList :: IO [(String,String)]
getPackageList = do
    all <- doBrewCmd ["list", "-1","--versions"]
    pure $ mapMaybe splitAtFirstSpace all

packageInfo :: String -> IO Package
packageInfo package = do
  entries <- doBrewCmd ["info", package]
  let (first:desc':url:others) = entries
  let [name', version'] = splitColon first
  let build' = findAndSplit "Build: " others
  let reqlist = findAndSplit "Required: " others
  return $
    Package
      {name = name', version = version', description = desc', link = parseURI url, build = build', required = reqlist}
