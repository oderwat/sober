{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text    (Text (..))
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Brewer                         ( getPackageList
                                                , packageInfo
                                                )

fShow :: (Text -> IO d) -> Text -> IO d
fShow func name = do
  T.putStrLn name
  func name

main :: IO ()
main = do
  packageList <- fmap (take 3) getPackageList
  putStrLn $ "Having " ++ show (length packageList) ++ " packages"
  alle <- mapM (fShow packageInfo . fst) packageList
  mapM_ print alle
