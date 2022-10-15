{-# OPTIONS_HADDOCK prune #-}

module Data.PerfectHash.DebugUtil where

import Data.List (intercalate)

-- | For debugging
showPairs :: [(String, String)] -> String
showPairs pairs = intercalate "; " $ map colonate pairs
  where
    colonate (k, v) = unwords [k <> ":", show v] 
