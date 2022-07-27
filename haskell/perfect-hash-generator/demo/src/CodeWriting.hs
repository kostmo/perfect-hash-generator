module CodeWriting where

import Data.List                    (intercalate)
import qualified Data.Vector      as Vector
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

import qualified Data.PerfectHash.Lookup  as Lookup


bracket :: String -> String
bracket x = "[" <> x <> "]"


curly :: String -> String
curly x = "{" <> x <> "}"


renderLookupTableCode :: Lookup.LookupTable Integer -> String
renderLookupTableCode table = unlines [
      "#include \"generated_lookup.h\""
    , ""
    , "const Fnv32_t MY_NONCES[] = " <> nonces_line <> ";"
    , "const size_t MY_SIZE = " <> show elem_count <> ";"
    ]
  
  where
    elem_count = length $ Lookup.values table
    nonces_line = curly $ intercalate ", " $ map show $ Vector.toList $ Lookup.nonces table


renderValuesTableCode :: Lookup.LookupTable Integer -> String
renderValuesTableCode table = unlines [
      "#include \"generated_values.h\""
    , ""
    , "const GENERATED_VALUES_TYPE HASHED_VALUES[] = " <> values_line <> ";"
    ]

  where
    values_line = curly $ intercalate ", " $ map show $ Vector.toList $ Lookup.values table


writeAllFiles :: Lookup.LookupTable Integer -> FilePath -> IO ()
writeAllFiles lookup_table outputDir = do
  createDirectoryIfMissing True outputDir

  writeFile (outputDir </> "generated_lookup.h") $ unlines [
      "#include \"fnv.h\""
    , ""
    , "extern const size_t MY_SIZE;"
    , "extern const Fnv32_t MY_NONCES[];"
    ]

  writeFile (outputDir </> "generated_lookup.c") rendered_lookup_table_code

  writeFile (outputDir </> "generated_values.h") $ unlines [
      "#define GENERATED_VALUES_TYPE int"
    , ""
    , "extern const GENERATED_VALUES_TYPE HASHED_VALUES[];"
    ]

  writeFile (outputDir </> "generated_values.c") rendered_values_table_code
  where
    rendered_lookup_table_code = CodeWriting.renderLookupTableCode lookup_table 
    rendered_values_table_code = CodeWriting.renderValuesTableCode lookup_table 
