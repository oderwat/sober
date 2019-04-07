{-# LANGUAGE OverloadedStrings #-}

module Brewer
  ( getPackageList
  , packageInfo
  , doBrewCmd
  ) where

import Data.Text (Text(..))
import qualified Data.Text as T

import Control.Applicative
import Data.Maybe
import Network.URI (URI, parseURI)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)

import Brewer.Internal

brewBin = "/usr/local/bin/brew"

data Package = Package
  { name :: Text
  , version :: Text
  , description :: Text
  , link :: Maybe URI
  , build :: [Text]
  , required :: [Text]
  } deriving (Show)

doBrewCmd :: [Text] -> IO [Text]
doBrewCmd args = do
  let cmdin = ""
  (errCode, cmdout, cmderr) <- readProcessWithExitCode brewBin (map T.unpack args) cmdin
  return $
    case errCode of
      ExitSuccess -> T.lines $ T.pack cmdout
      _ -> []

getPackageList :: IO [(Text, Text)]
getPackageList = do
  all <- doBrewCmd ["list", "-1", "--versions"]
  pure $ mapMaybe splitAtFirstSpace all

--  doBrewCmd ["list", "-1","--versions"] >>= (\t -> pure $ map separateNameVersion t)
--
--  fmap (map separateNameVersion) $ doBrewCmd ["list", "-1","--versions"]
--
packageInfo :: Text -> IO Package
packageInfo package = do
  entries <- doBrewCmd ["info", package]
  let (first:desc':url:others) = entries
  let [name', version'] = splitColon first
  let build' = findAndSplit "Build: " others
  let reqlist = findAndSplit "Required: " others
  return $
    Package
      {name = name', version = version', description = desc', link = parseURI $ T.unpack url, build = build', required = reqlist}
