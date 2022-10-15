module CodeWriting where

import Data.List                    (intercalate)
import qualified Data.Vector      as Vector
import Text.Read                   (readEither)
import System.FilePath ((</>), (<.>), takeDirectory)
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


quote :: String -> String
quote x = "\"" <> x <> "\""


semi :: String -> String
semi x = x <> ";"


renderLookupTableCode :: String -> Lookup.LookupTable Integer -> String
renderLookupTableCode filename_stem table = unlines [
      unwords ["#include", quote (filename_stem <.> "h")]
    , ""
    , semi $ "const Fnv32_t MY_NONCES[] = " <> nonces_line
    , semi $ "const size_t MY_SIZE = " <> show elem_count
    ]
  
  where
    elem_count = length $ Lookup.values table
    nonces_line = curly $ intercalate ", " $ map show $ Vector.toList $ Lookup.nonces table


renderValuesTableCode :: String -> Lookup.LookupTable Integer -> String
renderValuesTableCode filename_stem table = unlines [
      unwords ["#include", quote (filename_stem <.> "h")]
    , ""
    , semi $ "const GENERATED_VALUES_TYPE HASHED_VALUES[] = " <> values_line
    ]

  where
    values_line = curly $ intercalate ", " $
      map show $ Vector.toList $ Lookup.values table


writeLookupFilePair :: KeyType -> Lookup.LookupTable Integer -> FilePath -> IO ()
writeLookupFilePair key_type lookup_table outputDir = do

  print "Q1"

  writeFile (outputDir </> filename_stem <.> "h") $ unlines [
      unwords ["#include", quote "fnv.h"]
    , ""
    , semi "extern const size_t MY_SIZE"
    , semi "extern const Fnv32_t MY_NONCES[]"
--    , unwords ["#define", "FNV_LOOKUP_FUNCTION", lookup_function_name]
    ]

  print "Q2"

  writeFile (outputDir </> filename_stem <.> "c") rendered_lookup_table_code

  print "Q3"
  where
    rendered_lookup_table_code = CodeWriting.renderLookupTableCode filename_stem lookup_table 
    filename_stem = "generated_lookup"

    _lookup_function_name = case key_type of
      IntKey -> "fnv_32a_numeric_buf"
      StringKey -> "fnv_32a_str"


writeValuesFilePair :: Lookup.LookupTable Integer -> FilePath -> IO ()
writeValuesFilePair lookup_table outputDir = do

  writeFile (outputDir </> filename_stem <.> "h") $ unlines [
      "#define GENERATED_VALUES_TYPE int"
    , ""
    , semi $ unwords [
        "extern"
      , "const"
      , "GENERATED_VALUES_TYPE"
      , "HASHED_VALUES" <> bracket (show elem_count)
      ]
    ]

  writeFile (outputDir </> filename_stem <.> "c") rendered_values_table_code

  where
    filename_stem = "generated_values"
    elem_count = length $ Lookup.values lookup_table
    rendered_values_table_code = CodeWriting.renderValuesTableCode filename_stem lookup_table 


writeAllFiles :: KeyType -> Lookup.LookupTable Integer -> FilePath -> IO ()
writeAllFiles key_type lookup_table outputDir = do
  createDirectoryIfMissing True outputDir

  print "Got here 1"
  writeLookupFilePair key_type lookup_table outputDir
  print "Got here 1a"
  writeValuesFilePair lookup_table outputDir
  print "Got here 1b"


genLookupTable :: KeyType -> FilePath -> IO (Lookup.LookupTable Integer)
genLookupTable keyType csvPath = do

  either_result_int <- InputParsing.parseCsv readEither csvPath :: IO (Either String [(Int, Integer)])
  either_result_string <- InputParsing.parseCsv pure csvPath :: IO (Either String [(String, Integer)])

  let int_map = makeMap either_result_int
      str_map = makeMap either_result_string

  print $ unwords ["str_map:", show str_map]

  let lookup_table = Exercise.eitherError $ case keyType of
        IntKey -> Construction.createMinimalPerfectHash int_map
        StringKey -> Construction.createMinimalPerfectHash str_map

  return lookup_table

  where
    makeMap :: (Ord a, Show a) => Either String [(a, Integer)] -> Map.Map a Integer
    makeMap either_result =
      Exercise.eitherError $ InputParsing.validateMap =<< either_result



genCode :: KeyType -> FilePath -> FilePath -> IO ()
genCode keyType csvPath outputDir = do

  lookup_table <- genLookupTable keyType csvPath

  CodeWriting.writeAllFiles keyType lookup_table outputDir

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
    demoStringMap :: Map.Map String Integer
    demoStringMap = Map.fromList [
      --   ("foo", 1) -- THESE TWO COLLIDE A
      -- , ("abc", 3) -- THESE TWO COLLIDE A
      -- , ("bar", 2)
      -- , ("xyz", 4)
      --   ( "fee", 1) -- THESE TWO COLLIDE B
      -- , ( "baa", 2) -- THESE TWO COLLIDE B

      --   ( "a", 1) -- THESE TWO COLLIDE C
      -- , ( "c", 2) -- THESE TWO COLLIDE C

      --   ( "a", 1) -- THESE TWO COLLIDE D
      -- , ( "e", 2) -- THESE TWO COLLIDE D
      

      --   ( "a", 1) -- THESE TWO COLLIDE E
      -- , ( "g", 2) -- THESE TWO COLLIDE E

        ( "c", 1) -- THESE TWO COLLIDE F
      , ( "g", 2) -- THESE TWO COLLIDE F
      ]

    file_contents = case keyType of
      IntKey -> renderFileContents $ map (\(k, v) -> (show k, show v)) $
        Map.toList $ Map.mapKeys (.&. bitmask) $ Exercise.mkIntMapTuples entryCount
        where
          bitmask = 2^(maxKeyByteCount * 8) - 1
      StringKey -> renderFileContents $ map (fmap show) $
        Map.toList demoStringMap
    
    renderFileContents :: [(String, String)] -> String
    renderFileContents my_tuples = unlines $
      map (\(k, v) -> intercalate "," [k, v]) my_tuples
        

