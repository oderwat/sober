{-# LANGUAGE OverloadedStrings #-}

module Brewer
  ( getPackageList
  , packageInfo
  , doBrewCmd
  , defaultPackage
  , Package(..)
  ) where

import Debug.Trace (trace)

import Control.Applicative
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.List
import Data.Maybe
import Data.Text (Text(..))
import qualified Data.Text as T
import Network.URI (URI, parseURI)

import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)

import Data.TCache
import Data.TCache.DefaultPersistence
import Data.Typeable

import Brewer.Internal

brewBin = "/usr/local/bin/brew"

data Package = Package
  { name :: Text
  , version :: Text
  , description :: Text
  , link :: Text --Maybe URI
  , build :: [Text]
  , required :: [Text]
  } deriving (Read, Show, Typeable)

defaultPackage =
  Package
    { name = undefined
    , version = undefined
    , description = undefined
    , link = undefined
    , build = undefined
    , required = undefined
    }

{--
instance Read URI where
  -- NOTICE: this is just a toy implementation
  readsPrec _ uri = trace (show uri) [(fromJust $ parseURI (take pos uri), drop pos uri)]
    where pos = fromJust $ elemIndex ',' uri
-}

instance Indexable Package where
  key Package {name = id} = "Packages/" <> T.unpack (T.replace "/" "#" id)

instance Serializable Package where
  serialize = pack . show
  deserialize = read . unpack

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
    Package {name = name', version = version', description = desc', link = url, build = build', required = reqlist}