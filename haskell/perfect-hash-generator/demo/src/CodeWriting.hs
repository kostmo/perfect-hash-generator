module CodeWriting where

import Data.List                    (intercalate)
import qualified Data.Vector      as Vector
import Text.Read                   (readEither)
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Map              as Map
import           Data.Bits            ((.&.))

import qualified InputParsing
import qualified Data.PerfectHash.Construction  as Construction
import qualified Data.PerfectHash.Lookup  as Lookup
import qualified Exercise


data KeyType
  = IntKey
  | StringKey


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
    values_line = curly $ intercalate ", " $
      map show $ Vector.toList $ Lookup.values table


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


genLookupTable :: KeyType -> FilePath -> IO (Lookup.LookupTable Integer)
genLookupTable keyType csvPath = do

  either_result_int <- InputParsing.parseCsv readEither csvPath :: IO (Either String [(Int, Integer)])
  either_result_string <- InputParsing.parseCsv pure csvPath :: IO (Either String [(String, Integer)])

  let my_map_int :: Map.Map Int Integer
      my_map_int = makeMap either_result_int

      my_map_string :: Map.Map String Integer
      my_map_string = makeMap either_result_string

      lookup_table = case keyType of
        IntKey -> Construction.createMinimalPerfectHash my_map_int
        StringKey -> Construction.createMinimalPerfectHash my_map_string

  return lookup_table

  where
    makeMap :: (Ord a, Show a) => Either String [(a, Integer)] -> Map.Map a Integer
    makeMap either_result = case either_map of
      Right x -> x
      Left y -> error y
      where
        either_map = InputParsing.validateMap =<< either_result


genCode :: KeyType -> FilePath -> FilePath -> IO ()
genCode keyType csvPath outputDir = do

  lookup_table <- genLookupTable keyType csvPath

  CodeWriting.writeAllFiles lookup_table outputDir

  putStrLn $ unwords ["Wrote code files to:", outputDir]


data MapGenerationParameters = MapGenerationParameters {
    keyType :: KeyType
  , maxKeyByteCount :: Int
  , entryCount :: Int
  }


genCsv :: MapGenerationParameters -> FilePath -> IO ()
genCsv (MapGenerationParameters keyType maxKeyByteCount entryCount) csvPath = do

  createDirectoryIfMissing True $ takeDirectory csvPath
  writeFile csvPath file_contents

  putStrLn $ unwords ["Wrote CSV file to:", csvPath]
  where
    file_contents = case keyType of
      IntKey -> renderFileContents $ map (\(k, v) -> (show k, show v)) $
        Map.toList $ Map.mapKeys (.&. bitmask) $ Exercise.mkIntMapTuples entryCount
        where 
          bitmask = 2^(maxKeyByteCount * 8) - 1
      StringKey -> renderFileContents $ map (\(k, v) -> (k, show v)) $
        Map.toList $ Map.fromList [("foo", 1), ("bar", 2), ("abc", 3)]
    
    renderFileContents :: [(String, String)] -> String
    renderFileContents my_tuples = unlines $
      map (\(k, v) -> intercalate "," [k, v]) my_tuples
        

