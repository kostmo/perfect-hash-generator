module CodeWriting where

import Data.Either (fromRight)
import Data.List                    (intercalate)
import qualified Data.Vector      as Vector
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import qualified Data.Map              as Map
import           Data.Bits            ((.&.))

import qualified InputParsing
import qualified Data.PerfectHash.Construction  as Construction
import qualified Data.PerfectHash.Lookup  as Lookup
import qualified Exercise


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
    , unwords [
        "extern"
      , "const"
      , "GENERATED_VALUES_TYPE"
      , "HASHED_VALUES" <> bracket (show elem_count) <> ";"
      ]
    ]

  writeFile (outputDir </> "generated_values.c") rendered_values_table_code
  where
    elem_count = length $ Lookup.values lookup_table
    rendered_lookup_table_code = CodeWriting.renderLookupTableCode lookup_table 
    rendered_values_table_code = CodeWriting.renderValuesTableCode lookup_table 


genCode :: FilePath -> FilePath -> IO ()
genCode csvPath outputDir = do
  either_result <- InputParsing.parseCsv csvPath
  let myMap = InputParsing.validateMap =<< either_result

  print myMap

  let lookup_table = Construction.createMinimalPerfectHash $ fromRight (error "Bad map") myMap

  CodeWriting.writeAllFiles lookup_table outputDir

  putStrLn $ unwords ["Wrote code files to:", outputDir]


data MapGenerationParameters = MapGenerationParameters {
    maxKeyByteCount :: Int
  , entryCount :: Int
  }


genCsv :: MapGenerationParameters -> FilePath -> FilePath -> IO ()
genCsv (MapGenerationParameters maxKeyByteCount entryCount) csvPath outputDir = do

  createDirectoryIfMissing True outputDir
  writeFile (outputDir </> csvPath) file_contents

  putStrLn $ unwords ["Wrote CSV file to:", csvPath]
  where
    int_map = Exercise.mkIntMapTuples entryCount
    
    bitmask = 2^(maxKeyByteCount*8) - 1
    modified_int_map = Map.mapKeys (.&. bitmask) int_map
    
    file_contents = unlines $
      map (\(k, v) -> intercalate "," $ map show [k, v]) $
        Map.toList modified_int_map

