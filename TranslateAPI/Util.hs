module Util (error)
  where

import Prelude hiding (error)
import System.Exit


error :: String -> IO a
error message = do
  putStrLn $ "Error: " ++ message
  _ <- exitFailure
  undefined
