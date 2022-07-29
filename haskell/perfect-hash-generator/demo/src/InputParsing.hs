module InputParsing where

import           Control.Monad     (unless)
import           Data.String.Utils (strip)
import Text.Read                   (readEither)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Map                    (Map)


splitTuple delimiter string =
  (key_string, val_string)
  where
  [key_string, val_string] = splitOn delimiter string


-- | Ensures all keys are unique before converting to Map
parseCsv :: Read b => (String -> Either String a) -> FilePath -> IO (Either String [(a, b)])
parseCsv key_parser filePath = do
  file_lines <- readFile filePath
  return $ mapM wrapped_splitter $ lines file_lines
  where
    wrapped_splitter line = case splitter line of
      Left e -> Left $ unwords ["On line:", line, ":", e]
      Right x -> Right x
    splitter line = do
      keyVal <- key_parser key_string
      valVal <- wrappedReadEither val_string
      return (keyVal, valVal)
      where
        (key_string, val_string) = splitTuple "," line


wrappedReadEither :: (Read a) => String -> Either String a
wrappedReadEither text = case myRead text of
    Left e -> Left $ unwords ["For text:", text, ":", e]
    Right x -> Right x
  where
    myRead str = readEither $ strip str


validateMap :: (Show a, Ord a) => [(a, Integer)] -> Either String (Map a Integer)
validateMap tuples = do
  unless (Map.null repeats) $
    Left $ unwords ["Repeated values:", show repeats]
  return $ Map.fromList tuples
  where
    binned = binTuples tuples
    repeats = Map.filter ((> 1) . length) binned


-- * Utils

binTuples
  :: (Foldable t, Ord a)
  => t (a, b)
  -> Map a [b]
binTuples = foldr f mempty
  where
    f = uncurry (Map.insertWith mappend) . fmap pure