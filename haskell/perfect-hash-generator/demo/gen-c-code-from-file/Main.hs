module Main where

import Data.Either (fromRight)

import Options.Applicative

import qualified InputParsing
import qualified Data.PerfectHash.Construction  as Construction

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
      ( long "debug"
      <> help "enable debug mode")


main :: IO ()
main = run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Test the hashing on strings"
     <> header "string test" )


run (DemoOptions csvPath outputDir _enableDebug) = do
  either_result <- InputParsing.parseCsv csvPath
  let myMap = do 
        result <- either_result
        InputParsing.validateMap result
  print myMap

  let lookup_table = Construction.createMinimalPerfectHash $ fromRight (error "Bad map") myMap

  CodeWriting.writeAllFiles lookup_table outputDir

  putStrLn "Done."

