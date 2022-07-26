module CodeWriting where

import Data.List                    (intercalate)
import qualified Data.Vector      as Vector
import qualified Data.PerfectHash.Lookup  as Lookup


bracket :: String -> String
bracket x = "[" <> x <> "]"


curly :: String -> String
curly x = "{" <> x <> "}"


renderCode :: Lookup.LookupTable Integer -> String
renderCode table = unlines [
      "#include \"lookup.h\""
    , ""
    , "const NonceValPair my_elems[" <> show elem_count <> "] = " <> pairs_line <> ";"
    , "const size_t my_size = " <> show elem_count <> ";"
--    , "LookupTable TABLE ="
--    , curly (intercalate "," $ map ("\t" <>) [size_line, ".elems = " <> pairs_line]) <> ";"
--    , curly (intercalate "," $ map ("\t" <>) [size_line, ".elems = my_elems"]) <> ";"

    ]
  
  where
    elem_count = length $ Lookup.values table



    pairs :: [(Int, Integer)]
    pairs = zip (Vector.toList $ Lookup.nonces table) (Vector.toList $ Lookup.values table)

    f (nonce, val) = 
     curly (intercalate "," $ map ("\t" <>) [".nonce = " <> show nonce, ".value = " <> show val])

    pairs_line = curly $ intercalate ", " $ map f pairs