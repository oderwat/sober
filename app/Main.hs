module Main where

import           Control.Concurrent

import           Brewer                         ( getPackageNames
                                                , packageInfo
                                                )

fShow :: (String -> IO d) -> String -> IO d
fShow func name = do
  putStrLn name
  func name

main :: IO ()
main = do
  packageNames <- fmap (take 3) getPackageNames
  putStrLn $ "Having " ++ show (length packageNames) ++ " packages"
  alle <- mapM (fShow packageInfo) packageNames
  mapM_ print alle
