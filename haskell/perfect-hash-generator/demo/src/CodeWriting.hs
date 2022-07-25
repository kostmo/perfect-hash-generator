module CodeWriting where

import Data.List                    (intercalate)
import qualified Data.Vector      as Vector
import qualified Data.PerfectHash.Lookup  as Lookup


bracket :: String -> String
bracket x = "[" <> x <> "]"


curly :: String -> String
curly x = "{" <> x <> "}"


renderCode :: Lookup.LookupTable Integer -> String
renderCode table =
  unlines [nonces_line, values_line]
  
  where
    values_line = unwords [
        "const"
      , "int"
      , "VALUES[]"
      , "="
      , values_array_literal
      ] <> ";"


    values_array_literal = 
      curly $ intercalate ", " $ map show $
        Vector.toList $ Lookup.values table

    nonces_line = unwords [
        "const"
      , "int"
      , "NONCES[]"
      , "="
      , nonces_array_literal
      ] <> ";"

    nonces_array_literal = 
      curly $ intercalate ", " $ map show $
        Vector.toList $ Lookup.nonces table