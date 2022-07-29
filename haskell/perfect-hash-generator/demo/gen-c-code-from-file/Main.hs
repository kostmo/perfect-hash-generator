module Main where


import Options.Applicative


import qualified CodeWriting


data DemoOptions = DemoOptions {
    keyType :: String
  , csvPath :: FilePath
  , outputDir :: FilePath
  , writeCsvFile :: Bool
  }


optionsParser :: Parser DemoOptions
optionsParser = DemoOptions
  <$> strOption
      ( long "key-type"
      <> help "Key type")
  <*> strOption
      ( long "csv-filepath"
      <> help "CSV path")
  <*> strOption
      ( long "output-dir"
      <> help "Generated C code output directory")
  <*> switch
      ( long "write-csv"
      <> help "Write a random CSV file")


main :: IO ()
main = run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Test the hashing on strings"
     <> header "string test" )


run (DemoOptions keyTypeStr csvPath outputDir shouldWriteCsv) = 
  if shouldWriteCsv
    then CodeWriting.genCsv data_parms csvPath
    else CodeWriting.genCode keyType csvPath outputDir
  where
    data_parms = CodeWriting.MapGenerationParameters
      keyType
      2
      7

    keyType = case keyTypeStr of
      "int"    -> CodeWriting.IntKey
      "string" -> CodeWriting.StringKey
      _ -> error "Accepted types are 'int' and 'string'."