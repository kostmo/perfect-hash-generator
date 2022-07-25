module Main where

import Data.Either (fromRight)

import Options.Applicative

import qualified InputParsing
import qualified Data.PerfectHash.Construction  as Construction

import qualified CodeWriting


data DemoOptions = DemoOptions {
    inputPath :: FilePath
  , outputPath :: FilePath
  , debugEnabled :: Bool
  }


optionsParser :: Parser DemoOptions
optionsParser = DemoOptions
  <$> strOption
      ( long "input-filepath"
      <> help "CSV path"
      <> value "lookup_table.c")
  <*> strOption
      ( long "output-filepath"
      <> help "Generated C code file path")
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


run (DemoOptions inputPath outputPath _enableDebug) = do
  either_result <- InputParsing.parseCsv inputPath
  let myMap = do 
        result <- either_result
        InputParsing.validateMap result
  print myMap

  let lookup_table = Construction.createMinimalPerfectHash $ fromRight (error "Bad map") myMap
      rendered_code = CodeWriting.renderCode lookup_table 

  writeFile outputPath rendered_code

  putStrLn "Done."

