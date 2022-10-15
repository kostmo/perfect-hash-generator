{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module RenderMatrix where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.BrewerSet


import qualified Data.Matrix           as M
import  Data.Matrix           (Matrix)

import qualified BitsHistogram


paddingSize :: Double
paddingSize = 5


colors :: [Kolor]
colors = brewerSet Pastel1 9


textEnvelope val = text val # fontSizeL 15 # withEnvelope (rect 120 30 :: Diagram B)


singleSquare :: BitsHistogram.MatrixVal -> Diagram B
singleSquare (BitsHistogram.MatrixVal val bias) =
  singleSquareColored colr line_weight $ show val
  where
    colr = getCellColor bias
    line_weight = case bias of
      BitsHistogram.Correlation _ -> none
      _ -> 4



singleSquareColored
  :: Colour Double
  -> Measure Double
  -> String
  -> Diagram B
singleSquareColored colr line_weight val = offsetWriting <> enclosure
  where
    enclosure = roundedRect 40 30 5 # fc colr # lw line_weight

    offsetWriting = translateY (negate 10) writing
    writing = text val # fontSizeL 20 # fc black


getCellColor :: BitsHistogram.BiasIndicator -> Kolor
getCellColor BitsHistogram.AlwaysSame = sRGB 1 0 0
getCellColor BitsHistogram.AlwaysDifferent = sRGB 0 0 1
getCellColor (BitsHistogram.Correlation val) = if val > 0
  then sRGB 1 (1 - val) (1 - val)
  else sRGB (1 + val) (1 + val) 1


makeMatrixRow :: [BitsHistogram.MatrixVal] -> Diagram B
makeMatrixRow vals =
  hsep paddingSize items
  where
    items = map singleSquare vals


myDiagram :: Matrix BitsHistogram.MatrixVal -> Diagram B
myDiagram input_matrix = vcat [
    actual_matrix
  , textEnvelope "value table"
  ]
  where
    actual_matrix = vsep paddingSize horizontalRows
    matrixRows = map makeMatrixRow $
      M.toLists input_matrix

    -- labelRow = map (singleSquare (colors !! 2) . show) [0..M.ncols input_matrix - 1]
    -- horizontalRows = labelRow : matrixRows
    horizontalRows = matrixRows
