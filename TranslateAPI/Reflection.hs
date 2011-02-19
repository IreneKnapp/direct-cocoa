module Reflection (loadReflectionData)
  where

import Types
import Util
-- import LibFFI


loadReflectionData :: [Framework] -> IO [Framework]
loadReflectionData frameworks = do
  return frameworks
