{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text    (Text (..))
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Network.URI (URI, parseURI)

import Control.Monad
import Control.Concurrent

import Data.Typeable
import Data.TCache
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery

import           Brewer                         ( getPackageList
                                                , packageInfo
                                                , defaultPackage
                                                , Package(..)
                                                )
--import Debug.Trace (trace)
trace _ x = x

fShow :: (Text -> Text -> IO d) -> Text -> Text -> IO d
fShow func name ver = do
  T.putStrLn $ name <> " " <> ver
  func name ver

workPackage :: Text -> Text -> IO Package
workPackage srch ver = do
  p <- getResource defaultPackage { name = srch }
  case p of
    Nothing -> do
      pkg' <- packageInfo srch
      let pkg = pkg' { version = ver }
      print $ "New: " <> name pkg <> " " <> version pkg
      withResource pkg $ const pkg
      return pkg
    Just pkg ->
      if version pkg /= ver
        then do
          pkg' <- packageInfo srch
          let pkg = pkg' { version = ver }
          print $ "Upd: " <> name pkg <> " " <> version pkg
          withResource pkg $ const pkg
          return pkg
        else do
          print $ "Old: " <> name pkg <> " " <> version pkg
          return pkg

main :: IO ()
main = do
  index version
  packageList <- fmap (take 20) getPackageList
  putStrLn $ "Having " ++ show (length packageList) ++ " packages"

  alle <- mapM (uncurry $ fShow workPackage) packageList

  syncCache
  --mapM_ print alle

  r <- atomically $ version .==. ("3.3.2" :: Text)
  print r

  threadDelay 1000000
