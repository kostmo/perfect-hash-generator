module CodeWriting where

import Data.List                    (intercalate)
--import qualified Data.Vector      as Vector
import qualified Data.PerfectHash.Lookup  as Lookup


bracket :: String -> String
bracket x = "[" <> x <> "]"


curly :: String -> String
curly x = "{" <> x <> "}"


renderCode :: Lookup.LookupTable Integer -> String
renderCode table = unlines [
      "#include \"lookup.h\""
    , ""
    , "LookupTable TABLE = {"
    , "\t" <> size_line
    , "\t" <> pairs_line
    , "};"
    ]
  
  where
    size_line = unwords [
        ".size"
      , "="
      , show $ length $ Lookup.values table
      ] <> ","

--    pairs = zip (Vector.toList $ Lookup.nonces table) (Vector.toList $ Lookup.values table)
    pairs :: [(Int, Integer)]
    pairs = []

    f (nonce, val) = unwords [
        -- "struct NonceValPair
      -- , {"
        "{"
      , ".nonce = " <> show nonce
      , ".value = " <> show val
      , "}"
      ]

    pairs_line = unwords [
        ".elems ="
      , curly $ intercalate ", " $ map f pairs
      ]