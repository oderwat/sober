module Main where

import           Control.Concurrent

import           Brewer                         ( getPackageList
                                                , packageInfo
                                                )

fShow :: (String -> IO d) -> String -> IO d
fShow func name = do
  putStrLn name
  func name

main :: IO ()
main = do
  packageList <- fmap (take 3) getPackageList
  putStrLn $ "Having " ++ show (length packageList) ++ " packages"
  alle <- mapM (fShow packageInfo . fst) packageList
  mapM_ print alle
