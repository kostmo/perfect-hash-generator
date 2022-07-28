module Main where


import Options.Applicative


import qualified CodeWriting


data DemoOptions = DemoOptions {
    csvPath :: FilePath
  , outputDir :: FilePath
  , writeCsvFile :: Bool
  }


optionsParser :: Parser DemoOptions
optionsParser = DemoOptions
  <$> strOption
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


run (DemoOptions csvPath outputDir shouldWriteCsv) = 
  if shouldWriteCsv
    then CodeWriting.genCsv data_parms csvPath
    else CodeWriting.genCode csvPath outputDir
  where
    data_parms = CodeWriting.MapGenerationParameters 2 7