module Main where

import System.Directory (getHomeDirectory)
import System.FilePath

main = do
  d <- getHomeDirectory
  writeFile (d </> "plop.txt") "Printed!"

