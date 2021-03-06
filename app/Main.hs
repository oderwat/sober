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
--trace _ x = x

fShow :: (Text -> Text -> IO d) -> Text -> Text -> IO d
fShow func name ver = do
  T.putStrLn $ name <> " " <> ver
  func name ver

workPackage :: Text -> Text -> IO Package
workPackage srch ver = do
  p <- getResource defaultPackage { pkgName = srch }
  case p of
    Nothing -> do
      pkg' <- packageInfo srch
      -- update package info with data from list
      -- because they can actually differ
      let pkg = pkg' { pkgVersion = ver, pkgName = srch }
      print $ "New: " <> pkgName pkg <> " " <> pkgVersion pkg
      withResource pkg $ const pkg
      return pkg
    Just pkg ->
      if pkgVersion pkg /= ver
        then do
          pkg' <- packageInfo srch
          let pkg = pkg' { pkgVersion = ver }
          print $ "Upd: " <> pkgName pkg <> " " <> pkgVersion pkg
          withResource pkg $ const pkg
          return pkg
        else do
          print $ "Old: " <> pkgName pkg <> " " <> pkgVersion pkg
          return pkg

main :: IO ()
main = do
  index pkgVersion
  packageList <- fmap (take 20) getPackageList
  putStrLn $ "Having " ++ show (length packageList) ++ " packages"

  alle <- mapM (uncurry $ fShow workPackage) packageList

  syncCache
  --mapM_ print alle

  r <- atomically $ pkgVersion .==. ("3.3.2" :: Text)
  print r

  r <- atomically $ pkgName .==. ("Ant" :: Text)
  print r

  threadDelay 1000000
