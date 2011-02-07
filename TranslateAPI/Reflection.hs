module Reflection (loadReflectionData)
  where

import Types
import Util


loadReflectionData :: [Framework] -> IO [Framework]
loadReflectionData frameworks = do
  return frameworks
