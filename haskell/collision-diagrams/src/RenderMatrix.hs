{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module RenderMatrix where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import qualified Data.Matrix           as M
import  Data.Matrix           (Matrix)

import qualified HashBitsUsageHistogram
import qualified CommonRendering


makeMatrixRow :: [HashBitsUsageHistogram.MatrixVal] -> Diagram B
makeMatrixRow vals =
  hsep CommonRendering.paddingSize items
  where
    items = map CommonRendering.singleSquare vals


hashBitsDiagram :: [Matrix HashBitsUsageHistogram.MatrixVal] -> Diagram B
hashBitsDiagram input_matrices = 
  vsep (4*CommonRendering.paddingSize) $ map f input_matrices
  where
    f input_matrix = vcat [
        actual_matrix
      , CommonRendering.textEnvelope "value table"
      ]
      where
        actual_matrix = vsep CommonRendering.paddingSize horizontalRows
        matrixRows = map makeMatrixRow $
          M.toLists input_matrix

        -- labelRow = map (singleSquare (colors !! 2) . show) [0..M.ncols input_matrix - 1]
        -- horizontalRows = labelRow : matrixRows
        horizontalRows = matrixRows
